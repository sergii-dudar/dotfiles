local M = {}

local stacktrace_ns = vim.api.nvim_create_namespace("StackTraceNs")
local common = require("utils.common-util")
local java_util = require("utils.java.java-common")
local jdtls_util = require("utils.java.jdtls-util")
local lsp_util = require("utils.lsp-util")

local project_link_color = "#1E90FF"
local ex_link_color = "#8A2BE2"
vim.api.nvim_set_hl(0, "TraceProjectClassHl", {
    fg = project_link_color,
    underline = true,
    bold = true,
})
vim.api.nvim_set_hl(0, "TraceNonProjectClassHl", {
    fg = ex_link_color,
    underline = true,
    bold = true,
})

function M.highlight_java_test_trace_lines(lines, trace_selected_line)
    trace_selected_line = (trace_selected_line and trace_selected_line - 1) or 0
    -- vim.notify("selected_line: " .. trace_selected_line)
    for i, line in ipairs(lines) do
        local trace = java_util.parse_java_class_trace_line(line)
        if trace then
            local line_ix = i - 1 + trace_selected_line
            local file_path = java_util.java_class_to_proj_path(trace.class_path)
            local hl_group = file_path and "TraceProjectClassHl" or "TraceNonProjectClassHl"
            -- local buffer = vim.api.nvim_get_current_buf()
            vim.api.nvim_buf_set_extmark(0, stacktrace_ns, line_ix, trace.line_start_position, {
                end_line = line_ix, -- only current line
                end_col = trace.line_end_position,
                hl_group = hl_group,
            })
        end
    end
end

---@param text string
function M.highlight_java_test_trace_text(text, selected_line)
    local lines = {}
    for line in text:gmatch("[^\n]+") do
        table.insert(lines, line)
    end

    M.highlight_java_test_trace_lines(lines, selected_line)
end

function M.highlight_java_test_trace(buffer)
    if vim.api.nvim_buf_is_valid(buffer) then
        local lines = vim.api.nvim_buf_get_lines(buffer, 0, -1, false)
        M.highlight_java_test_trace_lines(lines)
    end
end

function M.highlight_java_test_trace_current_buf()
    M.highlight_java_test_trace(vim.api.nvim_get_current_buf())
end

function M.highlight_java_trace_selected()
    local stack_trace = common.get_visual_selection()
    M.highlight_java_test_trace_text(stack_trace, vim.fn.getpos("v")[2])
end

---@type parce java trace to locations (local project files and load from jdtls in case lib refs)
local parse_java_stack_trace = function(trace, result_callback)
    local loc_result_items_map = {}
    local jdt_classes = {}

    -- for i, parsed in ipairs(java_util.parse_java_mvn_run_class_text(trace)) do
    local parsed_trace = java_util.parse_java_mvn_run_class_text(trace)
    for i, parsed in ipairs(parsed_trace) do
        local file_path = java_util.java_class_to_proj_path(parsed.class_path)
        if file_path then
            loc_result_items_map[i] = {
                filename = file_path,
                lnum = parsed.class_line_number,
                end_lnum = parsed.class_line_number,
                col = 1,
            }
        else
            table.insert(jdt_classes, parsed.class_path)
        end
    end

    if vim.tbl_isempty(jdt_classes) then
        local loc_result_items = {}
        local trace_number = 1
        for i = #parsed_trace, 1, -1 do
            local parsed = parsed_trace[i]
            local loc_item = loc_result_items_map[i]
            if loc_item then
                loc_item.text = string.format("( %s ) %s", trace_number, parsed.method)
                table.insert(loc_result_items, loc_item)
            end
            trace_number = trace_number + 1
        end
        result_callback(loc_result_items)
    else
        jdtls_util.jdt_load_unique_class_list(jdt_classes, function(jdt_results_items_map)
            local trace_number = 1
            local all_result_items = {}
            for i = #parsed_trace, 1, -1 do
                local parsed = parsed_trace[i]
                local loc_item = loc_result_items_map[i]
                local jdt_sym_loc_item = jdt_results_items_map[parsed.class_path]
                if loc_item then
                    loc_item.text = string.format("( %s ) %s", trace_number, parsed.method)
                    table.insert(all_result_items, loc_item)
                elseif jdt_sym_loc_item then
                    local jdt_item = vim.lsp.util.symbols_to_items({ jdt_sym_loc_item }, 0)[1]
                    jdt_item.text = string.format("( %s ) %s.%s", trace_number, parsed.class_path, parsed.method)
                    jdt_item.lnum = parsed.class_line_number
                    jdt_item.end_lnum = parsed.class_line_number
                    -- dd(jdt_item)
                    table.insert(all_result_items, jdt_item)
                else
                    vim.notify(string.format("⚠️ class %s was not found", parsed.class_path))
                end
                trace_number = trace_number + 1
            end
            -- dd(all_result_items)
            result_callback(all_result_items)
        end)
    end
end

function M.show_stack_trace_qflist(stack_trace)
    parse_java_stack_trace(stack_trace, function(trace_items)
        -- dd(trace_items)
        vim.fn.setqflist({}, "r", { title = "Trace Quickfix List", items = trace_items })
        vim.cmd("Trouble qflist toggle")
    end)
end

function M.parse_selected_trace_to_qflist()
    local stack_trace = common.get_visual_selection()
    M.show_stack_trace_qflist(stack_trace)
end

function M.parse_buffer_trace_to_qflist()
    local stack_trace = common.get_current_buffer_text()
    M.show_stack_trace_qflist(stack_trace)
end

function M.parse_current_line_trace_to_qflist()
    local stack_trace = common.get_line_under_cursor()
    M.show_stack_trace_qflist(stack_trace)
end

--- Find a normal editing window (listed, normal buftype, not current)
local function find_edit_win()
    local cur_win = vim.api.nvim_get_current_win()
    for _, win in ipairs(vim.api.nvim_tabpage_list_wins(0)) do
        if win ~= cur_win then
            local buf = vim.api.nvim_win_get_buf(win)
            local bt = vim.bo[buf].buftype
            if bt == "" and vim.bo[buf].buflisted then
                return win
            end
        end
    end
    return nil
end

function M.parse_trace_under_cursor_and_open_in_buffer()
    local trace_line = common.get_line_under_cursor()
    local parsed = java_util.parse_java_class_trace_line(trace_line)
    if not parsed then
        vim.notify(string.format("⚠️ cant parse java class from line:\n%s", trace_line))
        return
    end

    local edit_win = find_edit_win()
    if not edit_win then
        vim.cmd("above split")
        edit_win = vim.api.nvim_get_current_win()
    else
        vim.api.nvim_set_current_win(edit_win)
    end

    local file_path = java_util.java_class_to_proj_path(parsed.class_path)
    if file_path then
        vim.cmd(string.format("edit +%d %s", parsed.class_line_number, file_path))
    else
        local jdtls_client_id = lsp_util.get_client_id_by_name("jdtls")
        if jdtls_client_id then
            local current_buf_id = vim.api.nvim_get_current_buf()
            if not vim.lsp.buf_is_attached(current_buf_id, jdtls_client_id) then
                vim.lsp.buf_attach_client(current_buf_id, jdtls_client_id)
            end
            jdtls_util.jdt_open_class(parsed.class_path, parsed.class_line_number)
        else
            vim.notify(string.format("⚠️ JDTLS is not running to open %s", parsed.class_path))
        end
    end
end

return M