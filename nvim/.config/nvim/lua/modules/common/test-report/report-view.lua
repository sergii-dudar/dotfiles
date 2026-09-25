-- Generic tree view for the test-report module.
-- Renders results grouped by group → container → member (e.g. package → class → method,
-- or crate::module → submodule → test_fn). Language-specifics are looked up
-- from the registry by snapshot.filetype.
--
-- Layout follows neotest's summary window: a header with per-status counts, then a tree
-- drawn with `├ ╰ │` connectors, `╮` marking an expanded node and `─` a collapsed one or a
-- leaf, the status icon in a fixed column right after the connector, and durations
-- right-aligned in a dim column.

local registry = require("modules.common.test-report.registry")
local nio_util = require("utils.nio-util")
local log = require("utils.logging-util").new({
    name = "test-report-view",
    filename = "test-report.log",
    level = vim.log.levels.DEBUG,
})

local M = {}

---@class report_view.Config
---@field width integer          Fixed width of the tree split (columns).
---@field collapse_passed boolean Start containers whose tests all passed collapsed (failed ones stay open).
local config = {
    width = 65,
    collapse_passed = false,
}

---@param opts? report_view.Config
function M.setup(opts)
    config = vim.tbl_extend("force", config, opts or {})
end

-- Nerd-font glyphs by codepoint (avoids invisible/mangled literals in source).
-- Same status icons as the gutter signs placed by the core.
local icon = {
    passed = vim.fn.nr2char(0xEAB2), -- codicon pass
    failed = vim.fn.nr2char(0xEAB8), -- codicon error
    skipped = vim.fn.nr2char(0xEB32), -- codicon circle-slash
    running = vim.fn.nr2char(0xEB37), -- codicon debug-rerun (shown during a rerun)
    total = vim.fn.nr2char(0xEA79), -- codicon beaker (header total)
}

-- Tree drawing characters (neotest summary style, 1-column indent per level)
local glyph = {
    child = "├",
    last_child = "╰",
    indent = "│",
    last_indent = " ",
    expanded = "╮",
    collapsed = "─",
    leaf = "─",
}
local INITIAL_INDENT = " "

-- Highlight groups owned by this view. All are `default` links, so a colorscheme or the
-- user can override them; re-applied on ColorScheme since schemes clear highlights.
local hl = {
    passed = "TestReportPassed",
    failed = "TestReportFailed",
    skipped = "TestReportSkipped",
    running = "TestReportRunning",
    indent = "TestReportIndent",
    group = "TestReportGroup",
    container = "TestReportContainer",
    member_failed = "TestReportMemberFailed",
    time = "TestReportTime",
    title = "TestReportTitle",
    count = "TestReportCount",
    dim = "TestReportDim",
}
local hl_defaults = {
    TestReportPassed = "DiagnosticOk",
    TestReportFailed = "DiagnosticError",
    TestReportSkipped = "DiagnosticWarn",
    TestReportRunning = "DiagnosticInfo",
    TestReportIndent = "Comment",
    TestReportGroup = "Directory",
    TestReportContainer = "Type",
    TestReportMemberFailed = "DiagnosticError",
    TestReportTime = "Comment",
    TestReportTitle = "Title",
    TestReportCount = "Normal",
    TestReportDim = "Comment",
}
local function define_highlights()
    for name, link in pairs(hl_defaults) do
        vim.api.nvim_set_hl(0, name, { link = link, default = true })
    end
end
define_highlights()
vim.api.nvim_create_autocmd("ColorScheme", {
    group = vim.api.nvim_create_augroup("TestReportViewHighlights", { clear = true }),
    callback = define_highlights,
})

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
---@field test_count integer
---@field expanded boolean

---@class report_view.GroupNode
---@field name string                Display name (possibly compacted, e.g. "com.foo.svc").
---@field full_path string           Full dotted/colon path from root (for expansion state tracking).
---@field status "passed"|"failed"|"skipped"
---@field containers report_view.ContainerNode[]
---@field children report_view.GroupNode[]
---@field test_count integer
---@field expanded boolean

---@class report_view.LineInfo
---@field type "header"|"separator"|"blank"|"help"|"group"|"container"|"member"
---@field node? report_view.GroupNode|report_view.ContainerNode|report_view.MemberNode
---@field container_node? report_view.ContainerNode
---@field group_node? report_view.GroupNode

-- Fixed width of the tree split. Re-asserted when other windows close (e.g.
-- nvim-dap-ui panels) so the tree doesn't grow as freed columns redistribute.
local fix_width_group = vim.api.nvim_create_augroup("TestReportViewFixWidth", { clear = true })

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

--- failed if anything failed, passed if anything passed, skipped only when
--- everything was skipped (an all-skipped class must not show a green check).
---@param statuses string[]
---@return "passed"|"failed"|"skipped"
local function aggregate_status(statuses)
    local has_passed = false
    for _, s in ipairs(statuses) do
        if s == "failed" then
            return "failed"
        elseif s == "passed" then
            has_passed = true
        end
    end
    if has_passed or #statuses == 0 then
        return "passed"
    end
    return "skipped"
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
    local container_by_id = {} ---@type table<string, report_view.ContainerNode>

    for id, result in pairs(snapshot.results) do
        local container_id = id:match("^(.+)#(.+)$")
        if container_id then
            local display = adapter.id_to_display(id)
            local group_name = display.group or "(default)"
            if not group_containers[group_name] then
                group_containers[group_name] = {}
            end

            local cont = container_by_id[container_id]
            if not cont then
                cont = {
                    name = display.container,
                    container_id = container_id,
                    file_path = snapshot.container_files and snapshot.container_files[container_id],
                    members = {},
                    expanded = true,
                    time = 0,
                    test_count = 0,
                }
                container_by_id[container_id] = cont
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
            cont.test_count = cont.test_count + 1
        end
    end

    -- Sort members and compute container statuses
    for _, cont_list in pairs(group_containers) do
        for _, cont in ipairs(cont_list) do
            table.sort(cont.members, failed_first_cmp)
            cont.status = aggregate_status(vim.tbl_map(function(m)
                return m.status
            end, cont.members))
            if config.collapse_passed and cont.status ~= "failed" then
                cont.expanded = false
            end
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
                test_count = 0,
            }
            group_node.children = compact(current, full_path)
            local statuses = {}
            for _, cont in ipairs(group_node.containers) do
                table.insert(statuses, cont.status)
                group_node.test_count = group_node.test_count + cont.test_count
            end
            for _, child_group in ipairs(group_node.children) do
                table.insert(statuses, child_group.status)
                group_node.test_count = group_node.test_count + child_group.test_count
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
        local default_count = 0
        for _, c in ipairs(trie_root.containers) do
            default_count = default_count + c.test_count
        end
        local default_group = {
            name = "(default)",
            full_path = "(default)",
            status = aggregate_status(vim.tbl_map(function(c)
                return c.status
            end, trie_root.containers)),
            containers = trie_root.containers,
            children = {},
            expanded = true,
            test_count = default_count,
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

---@param t number|nil seconds
---@return string
local function fmt_time(t)
    if type(t) ~= "number" or t <= 0 then
        return ""
    end
    if t >= 1 then
        return string.format("%.2fs", t)
    end
    local ms = math.floor(t * 1000 + 0.5)
    if ms == 0 then
        return ""
    end
    return string.format("%dms", ms)
end

--- Append `suffix` right-aligned to `width` (display cells). Never truncates: when the text
--- is already too wide, the suffix simply follows after one space.
local function append_right(text, hls, suffix, width, hl_group)
    if suffix == "" then
        return text, hls
    end
    local pad = width - vim.api.nvim_strwidth(text) - vim.api.nvim_strwidth(suffix)
    text = text .. string.rep(" ", math.max(pad, 1))
    local start = #text
    text = text .. suffix
    table.insert(hls, { start, #text, hl_group })
    return text, hls
end

--- Identity of a rendered line, used to keep the cursor on the same node across refreshes.
---@param info report_view.LineInfo|nil
---@return string|nil
local function node_key(info)
    if not info or not info.node then
        return nil
    end
    if info.type == "member" then
        return "mem:" .. info.node.id
    elseif info.type == "container" then
        return "cnt:" .. info.node.container_id
    elseif info.type == "group" then
        return "grp:" .. info.node.full_path
    end
    return nil
end

local function status_icon(status, running)
    if running then
        return icon.running, hl.running
    end
    return icon[status] or icon.skipped, hl[status] or hl.skipped
end

local function render()
    local tree = state.tree
    if not tree then
        return {}, {}, {}
    end

    local width = config.width
    if state.winid and vim.api.nvim_win_is_valid(state.winid) then
        width = vim.api.nvim_win_get_width(state.winid)
    end
    local align_width = width - 1

    local lines = {}
    local line_map = {}
    local all_hls = {}

    local function add_line(text, info, hls)
        table.insert(lines, text)
        table.insert(line_map, info)
        if hls then
            local line_idx = #lines - 1
            for _, h in ipairs(hls) do
                table.insert(all_hls, { line_idx, h[1], h[2], h[3] })
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
    local running = state.running and vim.tbl_count(state.running) or 0

    -- Header: "<label>  <total> <passed> <failed> <skipped> [<running>]        <time>"
    local label = (state.adapter and state.adapter.display_name) or state.snapshot.filetype or "tests"
    local header_parts = {
        { INITIAL_INDENT .. label, hl.title },
        { "  " .. icon.total .. " " .. total, hl.count },
        { "  " .. icon.passed .. " " .. passed, hl.passed },
        { "  " .. icon.failed .. " " .. failed, hl.failed },
        { "  " .. icon.skipped .. " " .. skipped, hl.skipped },
    }
    if running > 0 then
        table.insert(header_parts, { "  " .. icon.running .. " " .. running, hl.running })
    end
    local header_text, header_hls = format_line(header_parts)
    header_text, header_hls = append_right(header_text, header_hls, fmt_time(total_time), align_width, hl.time)
    add_line(header_text, { type = "header" }, header_hls)
    add_line("", { type = "blank" })

    if total == 0 then
        add_line(INITIAL_INDENT .. "No test results", { type = "blank" }, { { 0, #INITIAL_INDENT + 15, hl.dim } })
        return lines, line_map, all_hls
    end

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

    ---@param cont report_view.ContainerNode
    ---@param prefix string   Connector prefix for the container line itself
    ---@param branch string   `├` or `╰`
    ---@param child_pfx string Prefix for the member lines
    ---@param group report_view.GroupNode
    local function render_container(cont, prefix, branch, child_pfx, group)
        local marker = cont.expanded and glyph.expanded or glyph.collapsed
        local cnt_icon, cnt_icon_hl = status_icon(cont.status, is_running_cnt[cont.container_id])
        local parts = {
            { prefix .. branch .. marker, hl.indent },
            { " " .. cnt_icon .. " ", cnt_icon_hl },
            { cont.name, hl.container },
        }
        if not cont.expanded then
            table.insert(parts, { " (" .. cont.test_count .. ")", hl.dim })
        end
        local text, hls = format_line(parts)
        text, hls = append_right(text, hls, fmt_time(cont.time), align_width, hl.time)
        add_line(text, { type = "container", node = cont, group_node = group }, hls)

        if cont.expanded then
            for mem_idx, mem in ipairs(cont.members) do
                local mem_branch = (mem_idx == #cont.members) and glyph.last_child or glyph.child
                local mem_running = state.running and state.running[mem.id]
                local mem_icon, mem_icon_hl = status_icon(mem.status, mem_running)
                local mem_name_hl = mem.status == "failed" and hl.member_failed or nil
                local mem_text, mem_hls = format_line({
                    { child_pfx .. mem_branch .. glyph.leaf, hl.indent },
                    { " " .. mem_icon .. " ", mem_icon_hl },
                    { mem.name, mem_name_hl },
                })
                mem_text, mem_hls = append_right(mem_text, mem_hls, fmt_time(mem.time), align_width, hl.time)
                add_line(mem_text, { type = "member", node = mem, container_node = cont, group_node = group }, mem_hls)
            end
        end
    end

    -- Recursive rendering of a group line and (when expanded) its children
    -- (containers first, then sub-groups), all as siblings with tree connectors.
    ---@param group report_view.GroupNode
    ---@param prefix string
    ---@param branch string
    ---@param child_pfx string
    local function render_group(group, prefix, branch, child_pfx)
        local marker = group.expanded and glyph.expanded or glyph.collapsed
        local grp_icon, grp_icon_hl = status_icon(group.status, is_running_grp[group.full_path])
        local parts = {
            { prefix .. branch .. marker, hl.indent },
            { " " .. grp_icon .. " ", grp_icon_hl },
            { group.name, hl.group },
        }
        if not group.expanded then
            table.insert(parts, { " (" .. group.test_count .. ")", hl.dim })
        end
        local text, hls = format_line(parts)
        add_line(text, { type = "group", node = group }, hls)

        if not group.expanded then
            return
        end

        local items = {}
        for _, cont in ipairs(group.containers) do
            table.insert(items, { kind = "container", cont = cont })
        end
        for _, child in ipairs(group.children) do
            table.insert(items, { kind = "group", child = child })
        end

        for i, item in ipairs(items) do
            local is_last = i == #items
            local item_branch = is_last and glyph.last_child or glyph.child
            local item_child_pfx = child_pfx .. (is_last and glyph.last_indent or glyph.indent)
            if item.kind == "container" then
                render_container(item.cont, child_pfx, item_branch, item_child_pfx, group)
            else
                render_group(item.child, child_pfx, item_branch, item_child_pfx)
            end
        end
    end

    -- Top-level groups hang off the header like neotest's adapter root.
    for grp_idx, group in ipairs(tree) do
        local is_last = grp_idx == #tree
        local branch = is_last and glyph.last_child or glyph.child
        local child_pfx = INITIAL_INDENT .. (is_last and glyph.last_indent or glyph.indent)
        render_group(group, INITIAL_INDENT, branch, child_pfx)
    end

    return lines, line_map, all_hls
end

local function refresh()
    if not state.bufnr or not vim.api.nvim_buf_is_valid(state.bufnr) then
        return
    end

    -- Remember which node the cursor is on: failed-first sorting can reorder lines after
    -- a rerun, and the cursor must stay on the same test, not the same line number.
    local win_valid = state.winid and vim.api.nvim_win_is_valid(state.winid)
    local old_line, anchor
    if win_valid and state.line_map then
        old_line = vim.api.nvim_win_get_cursor(state.winid)[1]
        anchor = node_key(state.line_map[old_line])
    end

    local lines, line_map, highlights = render()
    state.line_map = line_map

    vim.bo[state.bufnr].modifiable = true
    vim.api.nvim_buf_set_lines(state.bufnr, 0, -1, false, lines)
    vim.bo[state.bufnr].modifiable = false

    vim.api.nvim_buf_clear_namespace(state.bufnr, ns, 0, -1)
    for _, h in ipairs(highlights) do
        vim.api.nvim_buf_set_extmark(state.bufnr, ns, h[1], h[2], {
            end_col = h[3],
            hl_group = h[4],
        })
    end

    if win_valid and old_line then
        local target = old_line
        if anchor then
            for i, info in ipairs(line_map) do
                if node_key(info) == anchor then
                    target = i
                    break
                end
            end
        end
        target = math.max(1, math.min(target, #lines))
        pcall(vim.api.nvim_win_set_cursor, state.winid, { target, 0 })
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

--- Positions are keyed by the id's member part (what find_test_positions returns),
--- which is NOT always the human display name (e.g. jest uses "L<row>" keys while
--- the display is "describe > title"). Always resolve through the id.
---@param mem report_view.MemberNode
---@return string
local function member_pos_key(mem)
    return (mem.id and mem.id:match("#(.+)$")) or mem.name
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
                line = positions[member_pos_key(info.node)]
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

    require("modules.common.test-report").show_output_for(info.node.name, info.node.result, {
        file_path = info.container_node.file_path,
        member_key = member_pos_key(info.node),
    })
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
            local pos_key = member_pos_key(info.node)
            if positions and pos_key and positions[pos_key] then
                vim.api.nvim_win_set_cursor(0, { positions[pos_key] + 1, 0 })
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
        if info.node.full_path == "(default)" then
            vim.notify("test-report: the default group has no package to rerun", vim.log.levels.WARN)
            return
        end
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
    { "cr",       "Go to test source" },
    { "o / gd",   "Go to test source" },
    { "O",        "Show test output" },
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

local help_ns = vim.api.nvim_create_namespace("test_report_view_help")

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

    for _, h in ipairs(hls) do
        vim.api.nvim_buf_set_extmark(help_buf, help_ns, h[1], h[2], { end_col = h[3], hl_group = h[4] })
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
    map("o", action_goto, "Go to test source")
    map("O", action_output, "Show test output")
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

    map("<MiddleMouse>", function()
        local pos = vim.fn.getmousepos()
        if pos.line > 0 then
            vim.api.nvim_win_set_cursor(0, { pos.line, 0 })
            action_toggle_fold()
        end
    end, "Toggle fold (mouse)")
end

function M.close()
    pcall(vim.api.nvim_clear_autocmds, { group = fix_width_group })
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
    vim.api.nvim_win_set_width(state.winid, config.width)

    vim.wo[state.winid].number = false
    vim.wo[state.winid].relativenumber = false
    vim.wo[state.winid].signcolumn = "no"
    vim.wo[state.winid].foldcolumn = "0"
    vim.wo[state.winid].wrap = false
    vim.wo[state.winid].list = false
    vim.wo[state.winid].spell = false
    vim.wo[state.winid].cursorline = true
    vim.wo[state.winid].winfixwidth = true
    vim.wo[state.winid].fillchars = "eob: " -- no `~` below the tree

    -- Keep the tree at a fixed width even when other splits open/close. When a
    -- vertical split (e.g. a nvim-dap-ui panel) closes, Neovim redistributes the
    -- freed columns and the tree can grow despite `winfixwidth`; snap it back.
    -- (This also undoes a manual resize of the tree; use `M.setup({ width = N })`.)
    vim.api.nvim_clear_autocmds({ group = fix_width_group })
    vim.api.nvim_create_autocmd({ "WinClosed", "WinResized" }, {
        group = fix_width_group,
        callback = function()
            if not (state.winid and vim.api.nvim_win_is_valid(state.winid)) then
                return
            end
            vim.schedule(function()
                if
                    state.winid
                    and vim.api.nvim_win_is_valid(state.winid)
                    and vim.api.nvim_win_get_width(state.winid) ~= config.width
                then
                    pcall(vim.api.nvim_win_set_width, state.winid, config.width)
                end
            end)
        end,
    })

    refresh()
    setup_keymaps(state.bufnr)

    vim.api.nvim_create_autocmd("BufWipeout", {
        buffer = state.bufnr,
        once = true,
        callback = function()
            pcall(vim.api.nvim_clear_autocmds, { group = fix_width_group })
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

---@return boolean
local function is_open()
    return state.bufnr ~= nil and vim.api.nvim_buf_is_valid(state.bufnr)
end

---@param snapshot test_report.Snapshot
function M.refresh_if_open(snapshot)
    if not is_open() then
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

--- Drop the "running" marks (rerun finished without a report, or was canceled).
function M.clear_running()
    if not state.running then
        return
    end
    state.running = nil
    if is_open() then
        refresh()
    end
end

--- Forget the view's copy of the results (the core cleared its state). The window stays
--- open showing an empty header; the next process() fills it through refresh_if_open().
function M.reset_if_open()
    if not is_open() then
        return
    end
    state.snapshot = {
        results = {},
        positions = {},
        container_files = {},
        filetype = state.snapshot and state.snapshot.filetype,
    }
    state.running = nil
    state.tree = {}
    refresh()
    log.info("tree view reset")
end

return M
