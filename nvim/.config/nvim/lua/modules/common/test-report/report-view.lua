-- Generic tree view for the test-report module.
-- Renders results grouped by group → container → member (e.g. package → class → method,
-- or crate::module → submodule → test_fn). Language-specifics are looked up
-- from the registry by snapshot.filetype.

local registry = require("modules.common.test-report.registry")
local nio_util = require("utils.nio-util")
local log = require("utils.logging-util").new({
    name = "test-report-view",
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
local INITIAL_INDENT = " "

local arrow_right = "▶"
local arrow_down = "▼"

local ns = vim.api.nvim_create_namespace("test_report_view")

---@class report_view.MemberNode
---@field name string
---@field status "passed"|"failed"|"skipped"
---@field time number|nil
---@field result test_report.TestResult
---@field id string

---@class report_view.ContainerNode
---@field name string                Display name (e.g. last segment of class FQN).
---@field container_id string        Full container identifier (e.g. "com.foo.Bar" or "crate::mod::tests").
---@field file_path string|nil
---@field status "passed"|"failed"|"skipped"
---@field time number
---@field members report_view.MemberNode[]
---@field expanded boolean

---@class report_view.GroupNode
---@field name string                Display name (possibly compacted, e.g. "com.foo.svc").
---@field full_path string           Full dotted/colon path from root (for expansion state tracking).
---@field status "passed"|"failed"|"skipped"
---@field containers report_view.ContainerNode[]
---@field children report_view.GroupNode[]
---@field expanded boolean

---@class report_view.LineInfo
---@field type "header"|"separator"|"blank"|"help"|"group"|"container"|"member"
---@field node? report_view.GroupNode|report_view.ContainerNode|report_view.MemberNode
---@field container_node? report_view.ContainerNode
---@field group_node? report_view.GroupNode

-- View state (singleton — only one tree view at a time)
local state = {
    bufnr = nil, ---@type integer|nil
    winid = nil, ---@type integer|nil
    prev_winid = nil, ---@type integer|nil
    tree = nil, ---@type report_view.GroupNode[]|nil
    line_map = nil, ---@type report_view.LineInfo[]|nil
    snapshot = nil, ---@type test_report.Snapshot|nil
    running = nil, ---@type table<string, boolean>|nil
    adapter = nil, ---@type test_report.LangAdapter|nil
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

--- Build hierarchical tree from flat `container_id#member` results.
--- Uses the adapter for id splitting + group separator (e.g. "." for java, "::" for rust).
---@param snapshot test_report.Snapshot
---@param adapter test_report.LangAdapter
---@return report_view.GroupNode[]
local function build_tree(snapshot, adapter)
    local group_sep = adapter.group_separator or "."

    -- Step 1: Collect containers grouped by group path
    local group_containers = {} ---@type table<string, report_view.ContainerNode[]>

    for id, result in pairs(snapshot.results) do
        local container_id = id:match("^(.+)#(.+)$")
        if container_id then
            local display = adapter.id_to_display(id)
            local group_name = display.group or "(default)"
            if not group_containers[group_name] then
                group_containers[group_name] = {}
            end

            local cont
            for _, c in ipairs(group_containers[group_name]) do
                if c.container_id == container_id then
                    cont = c
                    break
                end
            end
            if not cont then
                cont = {
                    name = display.container,
                    container_id = container_id,
                    file_path = snapshot.container_files and snapshot.container_files[container_id],
                    members = {},
                    expanded = true,
                    time = 0,
                }
                table.insert(group_containers[group_name], cont)
            end

            table.insert(cont.members, {
                name = display.member,
                status = result.status,
                time = result.time,
                result = result,
                id = id,
            })
            cont.time = (cont.time or 0) + (result.time or 0)
        end
    end

    -- Sort members and compute container statuses
    for _, cont_list in pairs(group_containers) do
        for _, cont in ipairs(cont_list) do
            table.sort(cont.members, failed_first_cmp)
            cont.status = aggregate_status(vim.tbl_map(function(m)
                return m.status
            end, cont.members))
        end
        table.sort(cont_list, failed_first_cmp)
    end

    -- Step 2: Build trie from group names
    local trie_root = { children_map = {}, containers = {} }
    for group_name, containers in pairs(group_containers) do
        if group_name == "(default)" then
            for _, cont in ipairs(containers) do
                table.insert(trie_root.containers, cont)
            end
        else
            local segments = vim.split(group_name, group_sep, { plain = true })
            local node = trie_root
            for _, seg in ipairs(segments) do
                if not node.children_map[seg] then
                    node.children_map[seg] = { children_map = {}, containers = {} }
                end
                node = node.children_map[seg]
            end
            node.containers = containers
        end
    end

    -- Step 3: Compact trie and convert to GroupNode[]
    local function compact(node, parent_path)
        local result = {}
        for seg, child in pairs(node.children_map) do
            local name_parts = { seg }
            local current = child
            while vim.tbl_count(current.children_map) == 1 and #current.containers == 0 do
                local next_seg, next_child = next(current.children_map)
                table.insert(name_parts, next_seg)
                current = next_child
            end
            local compacted_name = table.concat(name_parts, group_sep)
            local full_path = parent_path ~= "" and (parent_path .. group_sep .. compacted_name) or compacted_name
            local group_node = {
                name = compacted_name,
                full_path = full_path,
                status = "passed",
                containers = current.containers or {},
                children = {},
                expanded = true,
            }
            group_node.children = compact(current, full_path)
            local statuses = {}
            for _, cont in ipairs(group_node.containers) do
                table.insert(statuses, cont.status)
            end
            for _, child_group in ipairs(group_node.children) do
                table.insert(statuses, child_group.status)
            end
            if #statuses > 0 then
                group_node.status = aggregate_status(statuses)
            end
            table.insert(result, group_node)
        end
        table.sort(result, failed_first_cmp)
        return result
    end

    local groups = compact(trie_root, "")

    -- Handle default group containers
    if #trie_root.containers > 0 then
        table.sort(trie_root.containers, failed_first_cmp)
        local default_group = {
            name = "(default)",
            full_path = "(default)",
            status = aggregate_status(vim.tbl_map(function(c)
                return c.status
            end, trie_root.containers)),
            containers = trie_root.containers,
            children = {},
            expanded = true,
        }
        table.insert(groups, 1, default_group)
    end

    return groups
end

--- Recursively find the first container in a group subtree.
---@param group report_view.GroupNode
---@return report_view.ContainerNode|nil
local function find_first_container(group)
    if #group.containers > 0 then
        return group.containers[1]
    end
    for _, child in ipairs(group.children) do
        local cont = find_first_container(child)
        if cont then
            return cont
        end
    end
    return nil
end

--- Recursively collect all member IDs from a group subtree.
---@param group report_view.GroupNode
---@param ids table<string, boolean>
local function collect_member_ids(group, ids)
    for _, cont in ipairs(group.containers) do
        for _, mem in ipairs(cont.members) do
            ids[mem.id] = true
        end
    end
    for _, child in ipairs(group.children) do
        collect_member_ids(child, ids)
    end
end

--- Collect expansion state from the tree keyed by full_path/container_id.
local function collect_expansion_state(tree)
    local exp = {}
    local function walk(groups)
        for _, group in ipairs(groups) do
            exp["grp:" .. group.full_path] = group.expanded
            for _, cont in ipairs(group.containers) do
                exp["cnt:" .. cont.container_id] = cont.expanded
            end
            walk(group.children)
        end
    end
    walk(tree)
    return exp
end

--- Restore expansion state onto a rebuilt tree.
local function restore_expansion_state(tree, exp)
    local function walk(groups)
        for _, group in ipairs(groups) do
            local key = "grp:" .. group.full_path
            if exp[key] ~= nil then
                group.expanded = exp[key]
            end
            for _, cont in ipairs(group.containers) do
                local cnt_key = "cnt:" .. cont.container_id
                if exp[cnt_key] ~= nil then
                    cont.expanded = exp[cnt_key]
                end
            end
            walk(group.children)
        end
    end
    walk(tree)
end

local function rebuild_tree(old_tree, snapshot, adapter)
    local exp = collect_expansion_state(old_tree)
    local new_tree = build_tree(snapshot, adapter)
    restore_expansion_state(new_tree, exp)
    return new_tree
end

--- Build a line from parts, tracking highlight byte ranges.
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

    local summary_parts = {
        { " ❮❮❮", "GrayBold" },
    }
    table.insert(summary_parts, { " " .. tostring(total), "PurpleBold" })
    table.insert(summary_parts, { "/", "DiagnosticInfo" })
    table.insert(summary_parts, { tostring(passed), "DiagnosticOk" })
    table.insert(summary_parts, { "/", "DiagnosticInfo" })
    table.insert(summary_parts, { tostring(failed), "DiagnosticError" })
    table.insert(summary_parts, { "/", "DiagnosticInfo" })
    table.insert(summary_parts, { tostring(skipped), "DiagnosticWarn" })
    table.insert(summary_parts, { " · ", "GrenBold" })
    table.insert(summary_parts, { string.format("(%.2fs)", total_time), "Comment" })
    table.insert(summary_parts, { " ❯❯❯", "GrayBold" })

    local header_text, header_hls = format_line(summary_parts)
    add_line(header_text, { type = "header" }, header_hls)

    -- Pre-compute running state for containers and groups (bubbles up from member IDs)
    local is_running_cnt = {}
    local is_running_grp = {}
    if state.running then
        local function compute_running(groups)
            for _, group in ipairs(groups) do
                local grp_running = false
                for _, cont in ipairs(group.containers) do
                    for _, mem in ipairs(cont.members) do
                        if state.running[mem.id] then
                            is_running_cnt[cont.container_id] = true
                            grp_running = true
                            break
                        end
                    end
                end
                compute_running(group.children)
                for _, child in ipairs(group.children) do
                    if is_running_grp[child.full_path] then
                        grp_running = true
                    end
                end
                if grp_running then
                    is_running_grp[group.full_path] = true
                end
            end
        end
        compute_running(tree)
    end

    -- Recursive rendering of group children (sub-groups + containers)
    ---@param group report_view.GroupNode
    ---@param prefix string
    local function render_children(group, prefix)
        local items = {}
        for _, cont in ipairs(group.containers) do
            table.insert(items, { kind = "container", cont = cont })
        end
        for _, child in ipairs(group.children) do
            table.insert(items, { kind = "group", child = child })
        end

        for i, item in ipairs(items) do
            local is_last = i == #items
            local branch = is_last and BRANCH_LAST or BRANCH
            local cont_pfx = is_last and INDENT or CONTINUATION

            if item.kind == "container" then
                local cont = item.cont
                local cnt_fold = cont.expanded and arrow_down or arrow_right
                local cnt_running = is_running_cnt[cont.container_id]
                local cnt_icon = cnt_running and running_icon or result_icon[cont.status]
                local cnt_icon_hl = cnt_running and running_hl or result_hl[cont.status]
                local time_str = cont.time and string.format(" (%.2fs)", cont.time) or ""

                local cnt_text, cnt_hls = format_line({
                    { prefix .. branch, "Comment" },
                    { cnt_fold .. " ", "Comment" },
                    { cont.name, "Type" },
                    { " " .. cnt_icon, cnt_icon_hl },
                    { time_str, "Comment" },
                })
                add_line(cnt_text, { type = "container", node = cont, group_node = group }, cnt_hls)

                if cont.expanded then
                    for mem_idx, mem in ipairs(cont.members) do
                        local is_last_mem = mem_idx == #cont.members
                        local mem_branch = is_last_mem and BRANCH_LAST or BRANCH
                        local mem_running = state.running and state.running[mem.id]
                        local mem_icon = mem_running and running_icon or result_icon[mem.status]
                        local mem_icon_hl = mem_running and running_hl or result_hl[mem.status]
                        local mem_time = mem.time and string.format(" (%.3fs)", mem.time) or ""

                        local mem_name_hl = mem.status == "failed" and "DiagnosticError" or nil
                        local mem_text, mem_hls = format_line({
                            { prefix .. cont_pfx, "Comment" },
                            { mem_branch, "Comment" },
                            { mem.name, mem_name_hl },
                            { " " .. mem_icon, mem_icon_hl },
                            { mem_time, "Comment" },
                        })
                        add_line(
                            mem_text,
                            { type = "member", node = mem, container_node = cont, group_node = group },
                            mem_hls
                        )
                    end
                end
            else
                local child = item.child
                local fold_char = child.expanded and arrow_down or arrow_right
                local child_running = is_running_grp[child.full_path]
                local child_icon = child_running and running_icon or result_icon[child.status]
                local child_icon_hl = child_running and running_hl or result_hl[child.status]

                local child_text, child_hls = format_line({
                    { prefix .. branch, "Comment" },
                    { fold_char .. " ", "Comment" },
                    { child.name, "Directory" },
                    { " " .. child_icon, child_icon_hl },
                })
                add_line(child_text, { type = "group", node = child }, child_hls)

                if child.expanded then
                    render_children(child, prefix .. cont_pfx)
                end
            end
        end
    end

    -- Top-level groups
    for grp_idx, group in ipairs(tree) do
        local grp_running = is_running_grp[group.full_path]
        local icon = grp_running and running_icon or result_icon[group.status]
        local icon_hl = grp_running and running_hl or result_hl[group.status]
        local fold_char = group.expanded and arrow_down or arrow_right
        local grp_text, grp_hls = format_line({
            { INITIAL_INDENT .. fold_char .. " ", "Comment" },
            { group.name, "Directory" },
            { " " .. icon, icon_hl },
        })
        add_line(grp_text, { type = "group", node = group }, grp_hls)

        if group.expanded then
            render_children(group, INITIAL_INDENT)
        end

        if grp_idx < #tree then
            add_line("", { type = "blank" })
        end
    end

    return lines, line_map, all_hls
end

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

local function action_goto()
    local info = get_cursor_node()
    if not info then
        return
    end

    local file_path, line
    if info.type == "member" then
        file_path = info.container_node.file_path
        if file_path and state.snapshot.positions then
            local positions = state.snapshot.positions[file_path]
            if positions then
                line = positions[info.node.name]
            end
        end
    elseif info.type == "container" then
        file_path = info.node.file_path
    elseif info.type == "group" then
        local cont = find_first_container(info.node)
        if cont then
            file_path = cont.file_path
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

local function action_output()
    local info = get_cursor_node()
    if not info or info.type ~= "member" then
        vim.notify("test-report: select a test member to view output", vim.log.levels.INFO)
        return
    end

    require("modules.common.test-report").show_output_for(info.node.name, info.node.result)
end

local function action_toggle_fold()
    local info = get_cursor_node()
    if not info then
        return
    end

    if info.type == "group" or info.type == "container" then
        info.node.expanded = not info.node.expanded
        refresh()
    end
end

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

    if info.type == "member" then
        local file_path = info.container_node.file_path
        if not file_path then
            vim.notify("test-report: cannot resolve file for rerun", vim.log.levels.WARN)
            return
        end
        vim.api.nvim_set_current_win(win)
        vim.cmd("edit " .. vim.fn.fnameescape(file_path))
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
    elseif info.type == "container" then
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
        for _, mem in ipairs(info.node.members) do
            ids[mem.id] = true
        end
        state.running = ids
        refresh()
    elseif info.type == "group" then
        local first_container = find_first_container(info.node)
        if not first_container or not first_container.file_path then
            vim.notify("test-report: cannot resolve file for group rerun", vim.log.levels.WARN)
            return
        end
        vim.api.nvim_set_current_win(win)
        vim.cmd("edit " .. vim.fn.fnameescape(first_container.file_path))
        local group_name = info.node.full_path
        nio_util.run(function()
            require("plugins.overseer.overseer-util").run_test({
                test_type = task.test_type.ALL_DIR_TESTS,
                is_debug = is_debug,
                package_name = group_name,
            })
        end)
        local ids = {}
        collect_member_ids(info.node, ids)
        state.running = ids
        refresh()
    end
end

local function action_full_refresh()
    local snapshot = require("modules.common.test-report").get_report_snapshot()
    if vim.tbl_isempty(snapshot.results) then
        vim.notify("test-report: no test results available", vim.log.levels.WARN)
        return
    end
    local adapter = registry.get(snapshot.filetype)
    if not adapter then
        vim.notify("test-report: no adapter for filetype: " .. tostring(snapshot.filetype), vim.log.levels.WARN)
        return
    end
    state.adapter = adapter
    state.snapshot = {
        results = vim.deepcopy(snapshot.results),
        positions = snapshot.positions,
        container_files = vim.deepcopy(snapshot.container_files),
        filetype = snapshot.filetype,
    }
    state.running = nil
    state.tree = build_tree(state.snapshot, adapter)
    refresh()
    log.info("tree view full refresh")
end

---@param direction 1|-1
local function action_jump_failed(direction)
    if not state.line_map then
        return
    end
    local cur = vim.api.nvim_win_get_cursor(0)[1]
    local total = #state.line_map

    local function is_failed_member(idx)
        local info = state.line_map[idx]
        return info and info.type == "member" and info.node and info.node.status == "failed"
    end

    local i = cur + direction
    while i >= 1 and i <= total do
        if is_failed_member(i) then
            vim.api.nvim_win_set_cursor(0, { i, 0 })
            return
        end
        i = i + direction
    end

    i = direction == 1 and 1 or total
    while i ~= cur do
        if is_failed_member(i) then
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

    local help_ns = vim.api.nvim_create_namespace("test_report_view_help")
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

    map("<LeftRelease>", action_goto, "Go to test source (mouse)")
    map("<MiddleMouse>", function()
        local pos = vim.fn.getmousepos()
        if pos.line > 0 then
            vim.api.nvim_win_set_cursor(0, { pos.line, 0 })
            action_toggle_fold()
        end
    end, "Toggle fold (mouse)")
end

function M.close()
    if state.winid and vim.api.nvim_win_is_valid(state.winid) then
        vim.api.nvim_win_close(state.winid, true)
    end
    state.winid = nil
    state.bufnr = nil
end

---@param snapshot test_report.Snapshot
function M.open(snapshot)
    local adapter = registry.get(snapshot.filetype)
    if not adapter then
        vim.notify("test-report: no adapter for filetype: " .. tostring(snapshot.filetype), vim.log.levels.WARN)
        return
    end
    state.adapter = adapter
    state.snapshot = {
        results = vim.deepcopy(snapshot.results),
        positions = snapshot.positions,
        container_files = vim.deepcopy(snapshot.container_files),
        filetype = snapshot.filetype,
    }
    state.tree = build_tree(state.snapshot, adapter)
    state.prev_winid = vim.api.nvim_get_current_win()

    state.bufnr = vim.api.nvim_create_buf(false, true)
    vim.bo[state.bufnr].buftype = "nofile"
    vim.bo[state.bufnr].bufhidden = "wipe"
    vim.bo[state.bufnr].swapfile = false
    vim.bo[state.bufnr].filetype = "test-report-view"

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

    if state.line_map then
        for i, info in ipairs(state.line_map) do
            if info.type == "group" then
                vim.api.nvim_win_set_cursor(state.winid, { i, 0 })
                break
            end
        end
    end

    log.info("tree view opened with " .. #state.tree .. " groups")
end

---@param snapshot test_report.Snapshot
function M.toggle(snapshot)
    if state.bufnr and vim.api.nvim_buf_is_valid(state.bufnr) then
        M.close()
        return
    end
    M.open(snapshot)
end

---@param snapshot test_report.Snapshot
function M.refresh_if_open(snapshot)
    if not state.bufnr or not vim.api.nvim_buf_is_valid(state.bufnr) then
        return
    end
    local adapter = state.adapter or registry.get(snapshot.filetype)
    if not adapter then
        return
    end
    state.adapter = adapter
    for id, result in pairs(snapshot.results) do
        state.snapshot.results[id] = result
    end
    for container_id, file_path in pairs(snapshot.container_files or {}) do
        state.snapshot.container_files[container_id] = file_path
    end
    for file_path, positions in pairs(snapshot.positions or {}) do
        state.snapshot.positions[file_path] = positions
    end
    state.running = nil
    state.tree = rebuild_tree(state.tree, state.snapshot, adapter)
    refresh()
    log.info("tree view refreshed (incremental)")
end

return M
