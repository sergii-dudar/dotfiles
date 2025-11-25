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

M.highlight_java_test_trace = function(buffer)
    if vim.api.nvim_buf_is_valid(buffer) then
        local lines = vim.api.nvim_buf_get_lines(buffer, 0, -1, false)
        for i, line in ipairs(lines) do
            local trace = java_util.parse_java_mvn_run_class_line(line)
            if trace then
                local line_ix = i - 1
                local file_path = java_util.java_class_to_proj_path(trace.class_path)
                local hl_group = file_path and "TraceProjectClassHl" or "TraceNonProjectClassHl"
                vim.api.nvim_buf_set_extmark(buffer, stacktrace_ns, line_ix, trace.line_start_position, {
                    end_line = line_ix, -- only current line
                    end_col = trace.line_end_position,
                    hl_group = hl_group,
                })
            end
        end
    end
end

M.highlight_java_test_trace_current_buf = function()
    M.highlight_java_test_trace(vim.api.nvim_get_current_buf())
end

M.parse_java_stack_trace = function(trace)
    local items = {}

    -- for i, parsed in ipairs(java_util.parse_java_mvn_run_class_text(trace)) do
    local parsed_trace = java_util.parse_java_mvn_run_class_text(trace)
    local trace_number = 1
    for i = #parsed_trace, 1, -1 do
        local parsed = parsed_trace[i]
        local file_path = java_util.java_class_to_proj_path(parsed.class_path)
        --LazyVim.info("path: " .. file_path .. " file: " .. file)
        if file_path then
            table.insert(items, {
                filename = file_path,
                lnum = parsed.class_line_number,
                col = 1, -- Default to column 1
                --text = file .. " error location from stack trace"
                text = string.format("( %s ) %s", trace_number, parsed.method),
            })
            trace_number = trace_number + 1
        else
            -- TODO:
        end
    end

    return items
end

M.show_stack_trace_qflist = function(stack_trace)
    local trace_items = M.parse_java_stack_trace(stack_trace)
    dd(trace_items)
    vim.fn.setqflist({}, "r", { title = "Trace Quickfix List", items = trace_items })
    vim.cmd("Trouble qflist toggle")
end

M.parse_selected_trace_to_qflist = function()
    local stack_trace = common.get_visual_selection()
    M.show_stack_trace_qflist(stack_trace)
end

M.parse_buffer_trace_to_qflist = function()
    local stack_trace = common.get_current_buffer_text()
    M.show_stack_trace_qflist(stack_trace)
end

M.parse_current_line_trace_to_qflist = function()
    local stack_trace = common.get_line_under_cursor()
    M.show_stack_trace_qflist(stack_trace)
end

M.parse_trace_and_open_in_buffer = function()
    local trace_line = common.get_line_under_cursor()
    local parsed = java_util.parse_java_mvn_run_class_line(trace_line)
    if not parsed then
        vim.notify(string.format("⚠️ cant parse java class from line:\n%s", trace_line))
        return
    end
    local file_path = java_util.java_class_to_proj_path(parsed.class_path)
    if file_path then
        vim.cmd(string.format("wincmd k | l | edit +%d %s", parsed.class_line_number, file_path))
        -- vim.api.nvim_win_set_cursor(0, { tonumber(lnum), 0 })
    else
        -- vim.notify(string.format("⚠️ no local file found to open for %s", parsed.class_path))
        local jdtls_client_id = lsp_util.get_client_id_by_name("jdtls")
        if jdtls_client_id then
            local current_buf_id = vim.api.nvim_get_current_buf()
            if not vim.lsp.buf_is_attached(current_buf_id, jdtls_client_id) then
                vim.lsp.buf_attach_client(current_buf_id, jdtls_client_id)
                -- LazyVim.info("jdtls client found by ID:" .. jdtls_client_id)
                -- LazyVim.info("attaching jdtls to current buffer by ID:" .. current_buf_id)
            end
            vim.cmd("wincmd k | l")
            jdtls_util.jdt_open_class(parsed.class_path, parsed.class_line_number)
        else
            vim.notify(string.format("⚠️ JDTLS is not running to open %s", parsed.class_path))
        end
    end
end

return M
