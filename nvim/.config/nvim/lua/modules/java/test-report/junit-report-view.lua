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

-- Running indicator (shown during test rerun)
local running_icon = ""
local running_hl = "DiagnosticInfo"

-- Tree drawing characters
local BRANCH = "├─"
local BRANCH_LAST = "└─"
local CONTINUATION = "│ "
local INDENT = "  "

local arrow_right = "▶"
local arrow_down = "▼"
-- local arrow_right = "󰜴"
-- local arrow_down = "󰜮"

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
---@field full_path string
---@field status "passed"|"failed"|"skipped"
---@field classes report_view.ClassNode[]
---@field children report_view.PackageNode[]
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
    running = nil, ---@type table<string, boolean>|nil  -- set of method IDs currently being rerun
}

---@param statuses string[]
---@return "passed"|"failed"|"skipped"
local function aggregate_status(statuses)
    for _, s in ipairs(statuses) do
        if s == "failed" then
            return "failed"
        end
    end
    return "passed"
end

--- Sort comparator: failed first, then alphabetical by name.
---@param a { status: string, name: string }
---@param b { status: string, name: string }
---@return boolean
local function failed_first_cmp(a, b)
    if a.status ~= b.status then
        if a.status == "failed" then
            return true
        end
        if b.status == "failed" then
            return false
        end
    end
    return a.name < b.name
end

--- Build hierarchical tree from flat `classname#method` results.
--- Groups packages into a compacted trie: single-child intermediate nodes with no classes are collapsed.
---@param snapshot report_view.Snapshot
---@return report_view.PackageNode[]
local function build_tree(snapshot)
    -- Step 1: Collect classes grouped by package
    local pkg_classes = {} ---@type table<string, report_view.ClassNode[]>

    for id, result in pairs(snapshot.results) do
        local classname, method_name = id:match("^(.+)#(.+)$")
        if classname and method_name then
            local pkg_name = classname:match("^(.+)%.") or "(default)"
            if not pkg_classes[pkg_name] then
                pkg_classes[pkg_name] = {}
            end

            -- Find or create class entry
            local cls
            for _, c in ipairs(pkg_classes[pkg_name]) do
                if c.classname == classname then
                    cls = c
                    break
                end
            end
            if not cls then
                cls = {
                    name = classname:match("([^%.]+)$"),
                    classname = classname,
                    file_path = snapshot.class_files and snapshot.class_files[classname],
                    methods = {},
                    expanded = true,
                    time = 0,
                }
                table.insert(pkg_classes[pkg_name], cls)
            end

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

    -- Sort methods and compute class statuses
    for _, cls_list in pairs(pkg_classes) do
        for _, cls in ipairs(cls_list) do
            table.sort(cls.methods, failed_first_cmp)
            cls.status = aggregate_status(vim.tbl_map(function(m)
                return m.status
            end, cls.methods))
        end
        table.sort(cls_list, failed_first_cmp)
    end

    -- Step 2: Build trie from package names
    local trie_root = { children_map = {}, classes = {} }
    for pkg_name, classes in pairs(pkg_classes) do
        if pkg_name == "(default)" then
            for _, cls in ipairs(classes) do
                table.insert(trie_root.classes, cls)
            end
        else
            local segments = vim.split(pkg_name, ".", { plain = true })
            local node = trie_root
            for _, seg in ipairs(segments) do
                if not node.children_map[seg] then
                    node.children_map[seg] = { children_map = {}, classes = {} }
                end
                node = node.children_map[seg]
            end
            node.classes = classes
        end
    end

    -- Step 3: Compact trie and convert to PackageNode[]
    -- Collapses single-child chains with no classes into one node (e.g. "ua.raiffeisen.core")
    local function compact(node, parent_path)
        local result = {}
        for seg, child in pairs(node.children_map) do
            local name_parts = { seg }
            local current = child
            while vim.tbl_count(current.children_map) == 1 and #current.classes == 0 do
                local next_seg, next_child = next(current.children_map)
                table.insert(name_parts, next_seg)
                current = next_child
            end
            local compacted_name = table.concat(name_parts, ".")
            local full_path = parent_path ~= "" and (parent_path .. "." .. compacted_name) or compacted_name
            local pkg_node = {
                name = compacted_name,
                full_path = full_path,
                status = "passed",
                classes = current.classes or {},
                children = {},
                expanded = true,
            }
            pkg_node.children = compact(current, full_path)
            local statuses = {}
            for _, cls in ipairs(pkg_node.classes) do
                table.insert(statuses, cls.status)
            end
            for _, child_pkg in ipairs(pkg_node.children) do
                table.insert(statuses, child_pkg.status)
            end
            if #statuses > 0 then
                pkg_node.status = aggregate_status(statuses)
            end
            table.insert(result, pkg_node)
        end
        table.sort(result, failed_first_cmp)
        return result
    end

    local packages = compact(trie_root, "")

    -- Handle default package classes
    if #trie_root.classes > 0 then
        table.sort(trie_root.classes, failed_first_cmp)
        local default_pkg = {
            name = "(default)",
            full_path = "(default)",
            status = aggregate_status(vim.tbl_map(function(c)
                return c.status
            end, trie_root.classes)),
            classes = trie_root.classes,
            children = {},
            expanded = true,
        }
        table.insert(packages, 1, default_pkg)
    end

    return packages
end

--- Recursively find the first class in a package subtree.
---@param pkg report_view.PackageNode
---@return report_view.ClassNode|nil
local function find_first_class(pkg)
    if #pkg.classes > 0 then
        return pkg.classes[1]
    end
    for _, child in ipairs(pkg.children) do
        local cls = find_first_class(child)
        if cls then
            return cls
        end
    end
    return nil
end

--- Recursively collect all method IDs from a package subtree.
---@param pkg report_view.PackageNode
---@param ids table<string, boolean>
local function collect_method_ids(pkg, ids)
    for _, cls in ipairs(pkg.classes) do
        for _, meth in ipairs(cls.methods) do
            ids[meth.id] = true
        end
    end
    for _, child in ipairs(pkg.children) do
        collect_method_ids(child, ids)
    end
end

--- Collect expansion state from the tree keyed by full_path/classname.
---@param tree report_view.PackageNode[]
---@return table<string, boolean>
local function collect_expansion_state(tree)
    local exp = {}
    local function walk(packages)
        for _, pkg in ipairs(packages) do
            exp["pkg:" .. pkg.full_path] = pkg.expanded
            for _, cls in ipairs(pkg.classes) do
                exp["cls:" .. cls.classname] = cls.expanded
            end
            walk(pkg.children)
        end
    end
    walk(tree)
    return exp
end

--- Restore expansion state onto a rebuilt tree.
---@param tree report_view.PackageNode[]
---@param exp table<string, boolean>
local function restore_expansion_state(tree, exp)
    local function walk(packages)
        for _, pkg in ipairs(packages) do
            local key = "pkg:" .. pkg.full_path
            if exp[key] ~= nil then
                pkg.expanded = exp[key]
            end
            for _, cls in ipairs(pkg.classes) do
                local cls_key = "cls:" .. cls.classname
                if exp[cls_key] ~= nil then
                    cls.expanded = exp[cls_key]
                end
            end
            walk(pkg.children)
        end
    end
    walk(tree)
end

--- Rebuild tree from snapshot, preserving expansion state from previous tree.
---@param old_tree report_view.PackageNode[]
---@param snapshot report_view.Snapshot
---@return report_view.PackageNode[]
local function rebuild_tree(old_tree, snapshot)
    local exp = collect_expansion_state(old_tree)
    local new_tree = build_tree(snapshot)
    restore_expansion_state(new_tree, exp)
    return new_tree
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
    table.insert(summary_parts, { tostring(total) .. " tests: " })
    table.insert(summary_parts, { tostring(passed), "DiagnosticOk" })
    table.insert(summary_parts, { "/", "DiagnosticInfo" })
    table.insert(summary_parts, { tostring(failed), "DiagnosticError" })
    table.insert(summary_parts, { "/", "DiagnosticInfo" })
    table.insert(summary_parts, { tostring(skipped), "DiagnosticWarn" })
    table.insert(summary_parts, { string.format("  ·  %.2fs", total_time), "Comment" })

    local header_text, header_hls = format_line(summary_parts)
    add_line(header_text, { type = "header" }, header_hls)

    -- Separator
    local sep = string.rep("─", math.max(#header_text, 60))
    add_line(sep, { type = "separator" }, { { 0, #sep, "Comment" } })

    -- Blank line
    -- add_line("", { type = "blank" })

    -- Pre-compute running state for classes and packages (bubbles up from method IDs)
    local is_running_cls = {}
    local is_running_pkg = {}
    if state.running then
        local function compute_running(pkgs)
            for _, pkg in ipairs(pkgs) do
                local pkg_running = false
                for _, cls in ipairs(pkg.classes) do
                    for _, meth in ipairs(cls.methods) do
                        if state.running[meth.id] then
                            is_running_cls[cls.classname] = true
                            pkg_running = true
                            break
                        end
                    end
                end
                compute_running(pkg.children)
                for _, child in ipairs(pkg.children) do
                    if is_running_pkg[child.full_path] then
                        pkg_running = true
                    end
                end
                if pkg_running then
                    is_running_pkg[pkg.full_path] = true
                end
            end
        end
        compute_running(tree)
    end

    -- Recursive rendering of package children (sub-packages + classes)
    ---@param pkg report_view.PackageNode
    ---@param prefix string
    local function render_children(pkg, prefix)
        -- Combined child list: classes first, then sub-packages
        local items = {}
        for _, cls in ipairs(pkg.classes) do
            table.insert(items, { kind = "class", cls = cls })
        end
        for _, child in ipairs(pkg.children) do
            table.insert(items, { kind = "package", child = child })
        end

        for i, item in ipairs(items) do
            local is_last = i == #items
            local branch = is_last and BRANCH_LAST or BRANCH
            local cont = is_last and INDENT or CONTINUATION

            if item.kind == "class" then
                local cls = item.cls
                local cls_fold = cls.expanded and arrow_down or arrow_right
                local cls_running = is_running_cls[cls.classname]
                local cls_icon = cls_running and running_icon or result_icon[cls.status]
                local cls_icon_hl = cls_running and running_hl or result_hl[cls.status]
                local time_str = cls.time and string.format(" (%.2fs)", cls.time) or ""

                local cls_text, cls_hls = format_line({
                    { prefix .. branch, "Comment" },
                    { cls_fold .. " ", "Comment" },
                    { cls.name, "Type" },
                    { " " .. cls_icon, cls_icon_hl },
                    { time_str, "Comment" },
                })
                add_line(cls_text, { type = "class", node = cls, package_node = pkg }, cls_hls)

                if cls.expanded then
                    for meth_idx, meth in ipairs(cls.methods) do
                        local is_last_meth = meth_idx == #cls.methods
                        local meth_branch = is_last_meth and BRANCH_LAST or BRANCH
                        local meth_running = state.running and state.running[meth.id]
                        local meth_icon = meth_running and running_icon or result_icon[meth.status]
                        local meth_icon_hl = meth_running and running_hl or result_hl[meth.status]
                        local meth_time = meth.time and string.format(" (%.3fs)", meth.time) or ""

                        local meth_text, meth_hls = format_line({
                            { prefix .. cont, "Comment" },
                            { meth_branch, "Comment" },
                            { meth.name },
                            { " " .. meth_icon, meth_icon_hl },
                            { meth_time, "Comment" },
                        })
                        add_line(
                            meth_text,
                            { type = "method", node = meth, class_node = cls, package_node = pkg },
                            meth_hls
                        )
                    end
                end
            else
                local child = item.child
                local fold_char = child.expanded and arrow_down or arrow_right
                local child_running = is_running_pkg[child.full_path]
                local child_icon = child_running and running_icon or result_icon[child.status]
                local child_icon_hl = child_running and running_hl or result_hl[child.status]

                local child_text, child_hls = format_line({
                    { prefix .. branch, "Comment" },
                    { fold_char .. " ", "Comment" },
                    { child.name, "Directory" },
                    { " " .. child_icon, child_icon_hl },
                })
                add_line(child_text, { type = "package", node = child }, child_hls)

                if child.expanded then
                    render_children(child, prefix .. cont)
                end
            end
        end
    end

    -- Packages
    for pkg_idx, pkg in ipairs(tree) do
        local pkg_running = is_running_pkg[pkg.full_path]
        local icon = pkg_running and running_icon or result_icon[pkg.status]
        local icon_hl = pkg_running and running_hl or result_hl[pkg.status]
        local fold_char = pkg.expanded and arrow_down or arrow_right
        local pkg_text, pkg_hls = format_line({
            { fold_char .. " ", "Comment" },
            { pkg.name, "Directory" },
            { " " .. icon, icon_hl },
        })
        add_line(pkg_text, { type = "package", node = pkg }, pkg_hls)

        if pkg.expanded then
            render_children(pkg, "")
        end

        if pkg_idx < #tree then
            add_line("", { type = "blank" })
        end
    end

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
        local cls = find_first_class(info.node)
        if cls then
            file_path = cls.file_path
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
        state.running = { [info.node.id] = true }
        refresh()
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
        local ids = {}
        for _, meth in ipairs(info.node.methods) do
            ids[meth.id] = true
        end
        state.running = ids
        refresh()
    elseif info.type == "package" then
        local first_class = find_first_class(info.node)
        if not first_class or not first_class.file_path then
            vim.notify("test-report: cannot resolve file for package rerun", vim.log.levels.WARN)
            return
        end
        vim.api.nvim_set_current_win(win)
        vim.cmd("edit " .. vim.fn.fnameescape(first_class.file_path))
        local pkg_name = info.node.full_path
        nio_util.run(function()
            require("plugins.overseer.overseer-util").run_test({
                test_type = task.test_type.ALL_DIR_TESTS,
                is_debug = is_debug,
                package_name = pkg_name,
            })
        end)
        local ids = {}
        collect_method_ids(info.node, ids)
        state.running = ids
        refresh()
    end
end

--- Reload the tree from the current full snapshot (discards accumulated state).
local function action_full_refresh()
    local snapshot = require("modules.java.test-report").get_report_snapshot()
    if vim.tbl_isempty(snapshot.results) then
        vim.notify("test-report: no test results available", vim.log.levels.WARN)
        return
    end
    state.snapshot = {
        results = vim.deepcopy(snapshot.results),
        positions = snapshot.positions,
        class_files = vim.deepcopy(snapshot.class_files),
        filetype = snapshot.filetype,
    }
    state.running = nil
    state.tree = build_tree(state.snapshot)
    refresh()
    log.info("tree view full refresh")
end

--- Jump to next or previous failed test line.
---@param direction 1|-1
local function action_jump_failed(direction)
    if not state.line_map then
        return
    end
    local cur = vim.api.nvim_win_get_cursor(0)[1]
    local total = #state.line_map

    local function is_failed_method(idx)
        local info = state.line_map[idx]
        return info and info.type == "method" and info.node and info.node.status == "failed"
    end

    -- Search from cursor in direction
    local i = cur + direction
    while i >= 1 and i <= total do
        if is_failed_method(i) then
            vim.api.nvim_win_set_cursor(0, { i, 0 })
            return
        end
        i = i + direction
    end

    -- Wrap around
    i = direction == 1 and 1 or total
    while i ~= cur do
        if is_failed_method(i) then
            vim.api.nvim_win_set_cursor(0, { i, 0 })
            return
        end
        i = i + direction
    end
end

-- stylua: ignore start
local help_entries = {
    { "cr / LMB", "Go to test source" },
    { "gd",       "Go to test source" },
    { "o",        "Show test output" },
    { "r",        "Re-run test" },
    { "R",        "Debug test" },
    { "Tab / MMB","Toggle fold" },
    { "<leader>G","Full refresh" },
    { "]d",       "Next failed test" },
    { "[d",       "Prev failed test" },
    { "<leader>?","Show this help" },
    { "q",        "Close" },
}
-- stylua: ignore end

local function action_show_help()
    local lines = {}
    local hls = {}
    for _, entry in ipairs(help_entries) do
        local key, desc = entry[1], entry[2]
        local line = string.format("  %-12s %s", key, desc)
        table.insert(lines, line)
        table.insert(hls, { #lines - 1, 2, 2 + #key, "Special" })
    end

    local help_buf = vim.api.nvim_create_buf(false, true)
    vim.api.nvim_buf_set_lines(help_buf, 0, -1, false, lines)
    vim.bo[help_buf].modifiable = false
    vim.bo[help_buf].bufhidden = "wipe"

    local help_ns = vim.api.nvim_create_namespace("junit_report_help")
    for _, hl in ipairs(hls) do
        vim.api.nvim_buf_add_highlight(help_buf, help_ns, hl[4], hl[1], hl[2], hl[3])
    end

    local width = 42
    local height = #lines
    local win_opts = {
        relative = "win",
        win = state.winid,
        width = width,
        height = height,
        row = 1,
        col = 2,
        style = "minimal",
        border = "rounded",
        title = " Keybindings ",
        title_pos = "center",
    }
    local help_win = vim.api.nvim_open_win(help_buf, true, win_opts)
    vim.wo[help_win].cursorline = false

    vim.keymap.set("n", "q", function()
        if vim.api.nvim_win_is_valid(help_win) then
            vim.api.nvim_win_close(help_win, true)
        end
    end, { buffer = help_buf, silent = true, nowait = true })
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
    map("<leader>G", action_full_refresh, "Full refresh")
    map("]d", function()
        action_jump_failed(1)
    end, "Next failed test")
    map("[d", function()
        action_jump_failed(-1)
    end, "Prev failed test")
    map("<leader>?", action_show_help, "Show keybindings")
    map("q", function()
        M.close()
    end, "Close")

    -- Mouse: left click → goto, right click → fold/unfold
    map("<LeftRelease>", action_goto, "Go to test source (mouse)")
    map("<MiddleMouse>", function()
        local pos = vim.fn.getmousepos()
        if pos.line > 0 then
            vim.api.nvim_win_set_cursor(0, { pos.line, 0 })
            action_toggle_fold()
        end
    end, "Toggle fold (mouse)")
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
    -- Deep copy results and class_files so the tree owns its accumulated state
    state.snapshot = {
        results = vim.deepcopy(snapshot.results),
        positions = snapshot.positions,
        class_files = vim.deepcopy(snapshot.class_files),
        filetype = snapshot.filetype,
    }
    state.tree = build_tree(state.snapshot)
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

--- Incrementally merge new results into the tree view if it is currently open.
--- Rebuilds the tree preserving expansion state.
---@param snapshot report_view.Snapshot
function M.refresh_if_open(snapshot)
    if not state.bufnr or not vim.api.nvim_buf_is_valid(state.bufnr) then
        return
    end
    -- Merge results into accumulated snapshot
    for id, result in pairs(snapshot.results) do
        state.snapshot.results[id] = result
    end
    for classname, file_path in pairs(snapshot.class_files or {}) do
        state.snapshot.class_files[classname] = file_path
    end
    for file_path, positions in pairs(snapshot.positions or {}) do
        state.snapshot.positions[file_path] = positions
    end
    -- Clear running indicators now that new results have arrived
    state.running = nil
    -- Rebuild tree preserving expansion state
    state.tree = rebuild_tree(state.tree, state.snapshot)
    refresh()
    log.info("tree view refreshed (incremental)")
end

return M