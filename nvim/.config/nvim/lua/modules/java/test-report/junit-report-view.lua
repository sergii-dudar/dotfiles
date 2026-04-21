local nio_util = require("utils.nio-util")
local log = require("utils.logging-util").new({
    name = "junit-report-view",
    filename = "test-report.log",
    level = vim.log.levels.DEBUG,
})

local M = {}

-- Result icons placed after the name (signals pass/fail)
local result_icon = {
    passed = "",
    failed = "",
    skipped = "",
}
local result_hl = {
    passed = "DiagnosticOk",
    failed = "DiagnosticError",
    skipped = "DiagnosticWarn",
}

-- Tree drawing characters
local BRANCH = "├── "
local BRANCH_LAST = "└── "
local CONTINUATION = "│   "
local INDENT = "    "

local ns = vim.api.nvim_create_namespace("junit_report_view")

---@class report_view.Snapshot
---@field results table<string, test_report.TestResult>
---@field positions table<string, table<string, number>>
---@field class_files table<string, string>
---@field filetype string|nil

---@class report_view.MethodNode
---@field name string
---@field status "passed"|"failed"|"skipped"
---@field time number|nil
---@field result test_report.TestResult
---@field id string

---@class report_view.ClassNode
---@field name string
---@field classname string
---@field file_path string|nil
---@field status "passed"|"failed"|"skipped"
---@field time number
---@field methods report_view.MethodNode[]
---@field expanded boolean

---@class report_view.PackageNode
---@field name string
---@field status "passed"|"failed"|"skipped"
---@field classes report_view.ClassNode[]
---@field expanded boolean

---@class report_view.LineInfo
---@field type "header"|"separator"|"blank"|"help"|"package"|"class"|"method"
---@field node? report_view.PackageNode|report_view.ClassNode|report_view.MethodNode
---@field class_node? report_view.ClassNode
---@field package_node? report_view.PackageNode

-- View state (singleton — only one tree view at a time)
local state = {
    bufnr = nil, ---@type integer|nil
    winid = nil, ---@type integer|nil
    prev_winid = nil, ---@type integer|nil
    tree = nil, ---@type report_view.PackageNode[]|nil
    line_map = nil, ---@type report_view.LineInfo[]|nil
    snapshot = nil, ---@type report_view.Snapshot|nil
}

---@param statuses string[]
---@return "passed"|"failed"|"skipped"
local function aggregate_status(statuses)
    for _, s in ipairs(statuses) do
        if s == "failed" then
            return "failed"
        end
    end
    for _, s in ipairs(statuses) do
        if s == "skipped" then
            return "skipped"
        end
    end
    return "passed"
end

--- Build hierarchical tree from flat `classname#method` results.
---@param snapshot report_view.Snapshot
---@return report_view.PackageNode[]
local function build_tree(snapshot)
    local pkg_map = {}

    for id, result in pairs(snapshot.results) do
        local classname, method_name = id:match("^(.+)#(.+)$")
        if classname and method_name then
            local pkg_name = classname:match("^(.+)%.") or "(default)"
            local simple_class = classname:match("([^%.]+)$")

            if not pkg_map[pkg_name] then
                pkg_map[pkg_name] = { name = pkg_name, class_map = {}, expanded = true }
            end

            local pkg = pkg_map[pkg_name]
            if not pkg.class_map[classname] then
                pkg.class_map[classname] = {
                    name = simple_class,
                    classname = classname,
                    file_path = snapshot.class_files and snapshot.class_files[classname],
                    methods = {},
                    expanded = true,
                    time = 0,
                }
            end

            local cls = pkg.class_map[classname]
            table.insert(cls.methods, {
                name = method_name,
                status = result.status,
                time = result.time,
                result = result,
                id = id,
            })
            cls.time = (cls.time or 0) + (result.time or 0)
        end
    end

    -- Convert maps to sorted arrays, compute aggregate statuses
    local packages = {}
    for _, pkg in pairs(pkg_map) do
        local classes = {}
        for _, cls in pairs(pkg.class_map) do
            table.sort(cls.methods, function(a, b)
                if a.status ~= b.status then
                    if a.status == "failed" then
                        return true
                    end
                    if b.status == "failed" then
                        return false
                    end
                end
                return a.name < b.name
            end)
            cls.status = aggregate_status(vim.tbl_map(function(m)
                return m.status
            end, cls.methods))
            table.insert(classes, cls)
        end
        table.sort(classes, function(a, b)
            if a.status ~= b.status then
                if a.status == "failed" then
                    return true
                end
                if b.status == "failed" then
                    return false
                end
            end
            return a.name < b.name
        end)
        pkg.classes = classes
        pkg.class_map = nil
        pkg.status = aggregate_status(vim.tbl_map(function(c)
            return c.status
        end, classes))
        table.insert(packages, pkg)
    end

    table.sort(packages, function(a, b)
        if a.status ~= b.status then
            if a.status == "failed" then
                return true
            end
            if b.status == "failed" then
                return false
            end
        end
        return a.name < b.name
    end)

    return packages
end

--- Build a line from parts, tracking highlight byte ranges.
---@param parts { [1]: string, [2]: string|nil }[]
---@return string text
---@return { [1]: number, [2]: number, [3]: string }[] highlights {col_start, col_end, hl_group}
local function format_line(parts)
    local text = ""
    local hls = {}
    for _, part in ipairs(parts) do
        local start = #text
        text = text .. part[1]
        if part[2] then
            table.insert(hls, { start, #text, part[2] })
        end
    end
    return text, hls
end

--- Render the tree into lines + line_map + highlights.
---@return string[] lines
---@return report_view.LineInfo[] line_map
---@return { [1]: number, [2]: number, [3]: number, [4]: string }[] highlights {line, col_start, col_end, hl_group}
local function render()
    local tree = state.tree
    if not tree then
        return {}, {}, {}
    end

    local lines = {}
    local line_map = {}
    local all_hls = {}

    local function add_line(text, info, hls)
        table.insert(lines, text)
        table.insert(line_map, info)
        if hls then
            local line_idx = #lines - 1
            for _, hl in ipairs(hls) do
                table.insert(all_hls, { line_idx, hl[1], hl[2], hl[3] })
            end
        end
    end

    -- Summary counts
    local total, passed, failed, skipped, total_time = 0, 0, 0, 0, 0
    for _, result in pairs(state.snapshot.results) do
        total = total + 1
        if result.status == "passed" then
            passed = passed + 1
        elseif result.status == "failed" then
            failed = failed + 1
        elseif result.status == "skipped" then
            skipped = skipped + 1
        end
        total_time = total_time + (result.time or 0)
    end

    -- Header
    local summary_parts = { { "JUnit Test Report", "Title" }, { "  ·  " } }
    table.insert(summary_parts, { tostring(total) .. " tests" })
    if passed > 0 then
        table.insert(summary_parts, { "  " })
        table.insert(summary_parts, { " " .. tostring(passed), "DiagnosticOk" })
    end
    if failed > 0 then
        table.insert(summary_parts, { "  " })
        table.insert(summary_parts, { " " .. tostring(failed), "DiagnosticError" })
    end
    if skipped > 0 then
        table.insert(summary_parts, { "  " })
        table.insert(summary_parts, { " " .. tostring(skipped), "DiagnosticWarn" })
    end
    table.insert(summary_parts, { string.format("  ·  %.2fs", total_time), "Comment" })

    local header_text, header_hls = format_line(summary_parts)
    add_line(header_text, { type = "header" }, header_hls)

    -- Separator
    local sep = string.rep("─", math.max(#header_text, 60))
    add_line(sep, { type = "separator" }, { { 0, #sep, "Comment" } })

    -- Blank line
    add_line("", { type = "blank" })

    -- Packages
    for pkg_idx, pkg in ipairs(tree) do
        local icon = result_icon[pkg.status]
        local fold_char = pkg.expanded and "▼" or "▶"
        local pkg_text, pkg_hls = format_line({
            { fold_char .. " ", "Comment" },
            { pkg.name, "Directory" },
            { " " .. icon, result_hl[pkg.status] },
        })
        add_line(pkg_text, { type = "package", node = pkg }, pkg_hls)

        if pkg.expanded then
            for cls_idx, cls in ipairs(pkg.classes) do
                local is_last_cls = cls_idx == #pkg.classes
                local cls_branch = is_last_cls and BRANCH_LAST or BRANCH
                local cls_icon = result_icon[cls.status]
                local cls_fold = cls.expanded and "▼" or "▶"
                local time_str = cls.time and string.format(" (%.2fs)", cls.time) or ""

                local cls_text, cls_hls = format_line({
                    { cls_branch, "Comment" },
                    { cls_fold .. " ", "Comment" },
                    { cls.name, "Type" },
                    { " " .. cls_icon, result_hl[cls.status] },
                    { time_str, "Comment" },
                })
                add_line(cls_text, { type = "class", node = cls, package_node = pkg }, cls_hls)

                if cls.expanded then
                    local cont = is_last_cls and INDENT or CONTINUATION
                    for meth_idx, meth in ipairs(cls.methods) do
                        local is_last_meth = meth_idx == #cls.methods
                        local meth_branch = is_last_meth and BRANCH_LAST or BRANCH
                        local meth_icon = result_icon[meth.status]
                        local meth_time = meth.time and string.format(" (%.3fs)", meth.time) or ""

                        local meth_text, meth_hls = format_line({
                            { cont, "Comment" },
                            { meth_branch, "Comment" },
                            { meth.name },
                            { " " .. meth_icon, result_hl[meth.status] },
                            { meth_time, "Comment" },
                        })
                        add_line(
                            meth_text,
                            { type = "method", node = meth, class_node = cls, package_node = pkg },
                            meth_hls
                        )
                    end
                end
            end
        end

        if pkg_idx < #tree then
            add_line("", { type = "blank" })
        end
    end

    -- Help footer
    add_line("", { type = "blank" })
    local help = "cr:goto 󱋱 o:output 󱋱 r:run 󱋱 R:debug 󱋱 tab:fold 󱋱 q:close"
    add_line(help, { type = "help" }, { { 0, #help, "Comment" } })

    return lines, line_map, all_hls
end

--- Refresh buffer content from current tree state.
local function refresh()
    if not state.bufnr or not vim.api.nvim_buf_is_valid(state.bufnr) then
        return
    end

    local lines, line_map, highlights = render()
    state.line_map = line_map

    vim.bo[state.bufnr].modifiable = true
    vim.api.nvim_buf_set_lines(state.bufnr, 0, -1, false, lines)
    vim.bo[state.bufnr].modifiable = false

    vim.api.nvim_buf_clear_namespace(state.bufnr, ns, 0, -1)
    for _, hl in ipairs(highlights) do
        vim.api.nvim_buf_set_extmark(state.bufnr, ns, hl[1], hl[2], {
            end_col = hl[3],
            hl_group = hl[4],
        })
    end
end

--- Find the non-tree window to use for navigation and rerun.
---@return integer|nil
local function get_target_win()
    if state.prev_winid and vim.api.nvim_win_is_valid(state.prev_winid) then
        return state.prev_winid
    end
    for _, win in ipairs(vim.api.nvim_list_wins()) do
        if state.bufnr and vim.api.nvim_win_get_buf(win) ~= state.bufnr then
            return win
        end
    end
    return nil
end

---@return report_view.LineInfo|nil
local function get_cursor_node()
    if not state.line_map then
        return nil
    end
    local line = vim.api.nvim_win_get_cursor(0)[1]
    return state.line_map[line]
end

-- Navigate to test source file (and optionally to the method line).
local function action_goto()
    local info = get_cursor_node()
    if not info then
        return
    end

    local file_path, line
    if info.type == "method" then
        file_path = info.class_node.file_path
        if file_path and state.snapshot.positions then
            local positions = state.snapshot.positions[file_path]
            if positions then
                line = positions[info.node.name]
            end
        end
    elseif info.type == "class" then
        file_path = info.node.file_path
    elseif info.type == "package" then
        if info.node.classes and #info.node.classes > 0 then
            file_path = info.node.classes[1].file_path
        end
    end

    if not file_path then
        vim.notify("test-report: cannot resolve file", vim.log.levels.WARN)
        return
    end

    local win = get_target_win()
    if not win then
        return
    end
    vim.api.nvim_set_current_win(win)
    vim.cmd("edit " .. vim.fn.fnameescape(file_path))
    if line then
        vim.api.nvim_win_set_cursor(0, { line + 1, 0 })
    end
    vim.cmd("normal! zz")
end

-- Show test output for the method node under cursor.
local function action_output()
    local info = get_cursor_node()
    if not info or info.type ~= "method" then
        vim.notify("test-report: select a test method to view output", vim.log.levels.INFO)
        return
    end

    require("modules.java.test-report").show_output_for(info.node.name, info.node.result)
end

-- Toggle fold on package or class node.
local function action_toggle_fold()
    local info = get_cursor_node()
    if not info then
        return
    end

    if info.type == "package" or info.type == "class" then
        info.node.expanded = not info.node.expanded
        refresh()
    end
end

--- Rerun tests at cursor level (method/class/package).
--- Navigates to source file first so the existing test runner can resolve
--- classpath, module path, and JVM method signatures correctly.
---@param is_debug boolean|nil
local function action_rerun(is_debug)
    local info = get_cursor_node()
    if not info then
        return
    end

    local win = get_target_win()
    if not win then
        vim.notify("test-report: no target window for rerun", vim.log.levels.WARN)
        return
    end

    if info.type == "method" then
        local file_path = info.class_node.file_path
        if not file_path then
            vim.notify("test-report: cannot resolve file for rerun", vim.log.levels.WARN)
            return
        end
        vim.api.nvim_set_current_win(win)
        vim.cmd("edit " .. vim.fn.fnameescape(file_path))
        -- Position cursor at the test method for correct resolution
        if state.snapshot.positions then
            local positions = state.snapshot.positions[file_path]
            if positions and positions[info.node.name] then
                vim.api.nvim_win_set_cursor(0, { positions[info.node.name] + 1, 0 })
            end
        end
        nio_util.run(function()
            require("plugins.overseer.overseer-util").run_test({
                test_type = task.test_type.CURRENT_TEST,
                is_debug = is_debug,
            })
        end)
    elseif info.type == "class" then
        local file_path = info.node.file_path
        if not file_path then
            vim.notify("test-report: cannot resolve file for rerun", vim.log.levels.WARN)
            return
        end
        vim.api.nvim_set_current_win(win)
        vim.cmd("edit " .. vim.fn.fnameescape(file_path))
        nio_util.run(function()
            require("plugins.overseer.overseer-util").run_test({
                test_type = task.test_type.FILE_TESTS,
                is_debug = is_debug,
            })
        end)
    elseif info.type == "package" then
        local first_class = info.node.classes and info.node.classes[1]
        if not first_class or not first_class.file_path then
            vim.notify("test-report: cannot resolve file for package rerun", vim.log.levels.WARN)
            return
        end
        vim.api.nvim_set_current_win(win)
        vim.cmd("edit " .. vim.fn.fnameescape(first_class.file_path))
        nio_util.run(function()
            require("plugins.overseer.overseer-util").run_test({
                test_type = task.test_type.ALL_DIR_TESTS,
                is_debug = is_debug,
            })
        end)
    end
end

---@param buf integer
local function setup_keymaps(buf)
    local function map(lhs, fn, desc)
        vim.keymap.set("n", lhs, fn, { buffer = buf, silent = true, nowait = true, desc = desc })
    end

    map("<CR>", action_goto, "Go to test source")
    map("gd", action_goto, "Go to test source")
    map("o", action_output, "Show test output")
    map("r", function()
        action_rerun(false)
    end, "Re-run test")
    map("R", function()
        action_rerun(true)
    end, "Debug test")
    map("<Tab>", action_toggle_fold, "Toggle fold")
    map("q", function()
        M.close()
    end, "Close")
end

--- Close the tree view window.
function M.close()
    if state.winid and vim.api.nvim_win_is_valid(state.winid) then
        vim.api.nvim_win_close(state.winid, true)
    end
    state.winid = nil
    state.bufnr = nil
end

--- Open the tree view with the given report snapshot.
---@param snapshot report_view.Snapshot
function M.open(snapshot)
    state.snapshot = snapshot
    state.tree = build_tree(snapshot)
    state.prev_winid = vim.api.nvim_get_current_win()

    state.bufnr = vim.api.nvim_create_buf(false, true)
    vim.bo[state.bufnr].buftype = "nofile"
    vim.bo[state.bufnr].bufhidden = "wipe"
    vim.bo[state.bufnr].swapfile = false
    vim.bo[state.bufnr].filetype = "junit-report-view"

    vim.cmd("botright vsplit")
    state.winid = vim.api.nvim_get_current_win()
    vim.api.nvim_win_set_buf(state.winid, state.bufnr)
    vim.api.nvim_win_set_width(state.winid, 65)

    vim.wo[state.winid].number = false
    vim.wo[state.winid].relativenumber = false
    vim.wo[state.winid].signcolumn = "no"
    vim.wo[state.winid].foldcolumn = "0"
    vim.wo[state.winid].wrap = false
    vim.wo[state.winid].cursorline = true
    vim.wo[state.winid].winfixwidth = true

    refresh()
    setup_keymaps(state.bufnr)

    vim.api.nvim_create_autocmd("BufWipeout", {
        buffer = state.bufnr,
        once = true,
        callback = function()
            state.bufnr = nil
            state.winid = nil
        end,
    })

    -- Position cursor on first package line (after header + separator + blank)
    if state.line_map then
        for i, info in ipairs(state.line_map) do
            if info.type == "package" then
                vim.api.nvim_win_set_cursor(state.winid, { i, 0 })
                break
            end
        end
    end

    log.info("tree view opened with " .. #state.tree .. " packages")
end

--- Toggle the tree view: close if open, open if closed.
---@param snapshot report_view.Snapshot
function M.toggle(snapshot)
    if state.bufnr and vim.api.nvim_buf_is_valid(state.bufnr) then
        M.close()
        return
    end
    M.open(snapshot)
end

return M
