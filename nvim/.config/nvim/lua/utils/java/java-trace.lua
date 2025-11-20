local M = {}

local common = require("utils.common-util")

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

M.highlight_mvn_test_trace = function(buffer, namespace)
    if vim.api.nvim_buf_is_valid(buffer) then
        -- vim.notify("highlight", vim.log.levels.INFO)
        local lines = vim.api.nvim_buf_get_lines(buffer, 0, -1, false)

        for i, line in ipairs(lines) do
            if line:find("^%s*at ([%w%._$]+)%.([%w%._$]+)%(([%w%._$]+):(%d+)%)$") then
                local start_ix = string.find(line, "at") + 2
                -- vim.notify(line, vim.log.levels.INFO)
                local line_ix = i - 1
                vim.api.nvim_buf_set_extmark(buffer, namespace, line_ix, start_ix, {
                    end_line = line_ix, -- only current line
                    end_col = #line,
                    hl_group = "TraceProjectClassHl",
                })
            end
        end
    end
end

M.highlight_mvn_compile_trace = function() end

M.parse_java_stack_trace = function(trace)
    local items = {}

    -- Match patterns like "Class.method(FileName.java:LineNumber)"
    --for cp_path, method, file, line in string.gmatch(trace, "at (.*)%.([%w_-]+)%(([%w%.%/%_-]+%.java):(%d+)") do
    for cp_path, method, file, line in string.gmatch(trace, "([%w%.%/%_-]*)%.([%w_-]+)%(([%w%.%/%_-]+%.java):(%d+)") do
        local file_no_ex = string.match(file, "([^.]+)")
        cp_path = string.gsub(cp_path, "." .. file_no_ex, "")
        local path = string.gsub(cp_path, "%.", "/")
        --print(path .. ", " .. method .. ", " .. file .. ", " .. line)

        -- local file_path = vim.fn.findfile(file)
        -- resolve file full path from root
        local file_path = vim.fn.glob("*/**/" .. path .. "/" .. file)

        --LazyVim.info("path: " .. file_path .. " file: " .. file)
        if file_path ~= nil and #file_path ~= 0 then
            -- LazyVim.info(file .. ": '" .. file_path .. "'")

            table.insert(items, {
                filename = file_path,
                lnum = tonumber(line),
                col = 1, -- Default to column 1
                --text = file .. " error location from stack trace"
                text = method,
            })
        end
    end

    dd(items)
    return items
end

M.show_stack_trace_qflist = function(stack_trace)
    local trace_items = M.parse_java_stack_trace(stack_trace)
    -- dd(trace_items)
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

return M
