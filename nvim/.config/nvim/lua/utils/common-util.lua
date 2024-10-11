local M = {}

--[[
additional navigation:

1. Using Neovim's Built-in Navigation
Neovim has built-in commands to move through jump locations:
Ctrl+o: Move to the previous location (similar to "back" in IntelliJ).
Ctrl+i: Move to the next location (similar to "forward" in IntelliJ).

2.
gf - go to file under cursor,
gF - go to file under cursor and to number after : (File.java:20)

3.
:[line number] - got to line number in buffer

4.
cfdo %s/serhii_dudar/just_serhii/g | update | bd
cfdo %s/just_serhii/serhii_dudar/g | update | bd

]]

M.get_visual_selection = function()
    vim.cmd('noau normal! "vy"')
    local text = vim.fn.getreg('v')
    vim.fn.setreg('v', {})
    return text;
end

M.parse_java_stack_trace = function(trace)
    local items = {}

    -- Match patterns like "Class.method(FileName.java:LineNumber)"
    --for cp_path, method, file, line in string.gmatch(trace, "at (.*)%.([%w_-]+)%(([%w%.%/%_-]+%.java):(%d+)") do
    for cp_path, method, file, line in string.gmatch(trace, "([%w%.%/%_-]*)%.([%w_-]+)%(([%w%.%/%_-]+%.java):(%d+)") do

        local file_no_ex = string.match(file, "([^.]+)")
        cp_path = string.gsub(cp_path, "."..file_no_ex, "")
        local path = string.gsub(cp_path, "%.", "/")
        --print(path .. ", " .. method .. ", " .. file .. ", " .. line)

        -- local file_path = vim.fn.findfile(file)
        -- resolve file full path from root
        local file_path = vim.fn.glob("*/**/" .. path .. "/" ..file)

        --LazyVim.info("path: " .. file_path .. " file: " .. file)
        if file_path ~= nil and #file_path ~= 0 then
            -- LazyVim.info(file .. ": '" .. file_path .. "'")

            table.insert(items, {
                filename = file_path,
                lnum = tonumber(line),
                col = 1,  -- Default to column 1
                --text = file .. " error location from stack trace"
                text = method
            })
        end
    end

    return items
end

M.get_current_buffer_text = function()
    local current_buf_id = vim.api.nvim_get_current_buf()

    -- Get all lines from the current buffer
    local lines = vim.api.nvim_buf_get_lines(current_buf_id, 0, -1, false)

    -- Join the lines into a single string (optional)
    return table.concat(lines, "\n")
end

M.show_stack_trace_qflist = function(stack_trace)
    local trace_items = M.parse_java_stack_trace(stack_trace)
    vim.fn.setqflist({}, 'r', { title = 'Trace Quickfix List', items = trace_items })
    vim.cmd("Trouble qflist toggle")
end

M.get_file_with_line = function()
    -- Get the text under the cursor (assuming it's a file and line number)
    local cursor_word = vim.fn.expand('<cfile>')
    local full_word = vim.fn.expand("<cWORD>")

    local result_expr
    if tonumber(cursor_word) then
        result_expr = "[A-Za-z0-9_-]*%.?%w*:" .. cursor_word
    else
        result_expr = cursor_word .. ":?%d*"
    end

    local file = string.match(full_word, result_expr)
    return file
end

M.get_file_with_no_ext = function()
    local fileName = M.get_file_with_line()
    return string.match(fileName, "([^.]+)")
end

M.get_client_id_by_name = function(name)
    -- local clients = vim.lsp.buf_get_clients()
    local clients = vim.lsp.get_clients()

    for client_id, client in pairs(clients) do
        -- LazyVim.info("jdtls client:" .. client_id .. ", name: " .. client.name)
        if client.name == name then
            return client_id
        end
    end

    return nil  -- Return nil if no client with the specified name is found
end

M.table_to_string = function(tbl, indent)
    indent = indent or 0
    local to_log = string.rep(" ", indent) .. "{\n"

    for k, v in pairs(tbl) do
        local key = tostring(k)
        if type(v) == "table" then
            to_log = to_log .. string.rep(" ", indent + 2) .. key .. " = " .. M.table_to_string(v, indent + 2) .. ",\n"
        else
            local value = tostring(v)
            to_log = to_log .. string.rep(" ", indent + 2) .. key .. " = " .. value .. ",\n"
        end
    end

    to_log = to_log .. string.rep(" ", indent) .. "}"
    return to_log
end

M.log_table = function(table)
    local str_table = M.table_to_string(table)
    LazyVim.info("lua talbe: " .. str_table)
end

--lua print(vim.inspect(vim.lsp.get_active_clients()[1].name))

return M;