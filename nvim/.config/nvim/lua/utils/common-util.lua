local M = {}

M.get_visual_selection = function()
    vim.cmd('noau normal! "vy"')
    local text = vim.fn.getreg("v")
    vim.fn.setreg("v", {})
    return text
end

M.get_current_buffer_text = function()
    local current_buf_id = vim.api.nvim_get_current_buf()

    -- Get all lines from the current buffer
    local lines = vim.api.nvim_buf_get_lines(current_buf_id, 0, -1, false)

    -- Join the lines into a single string (optional)
    return table.concat(lines, "\n")
end

M.get_file_with_line = function()
    -- Get the text under the cursor (assuming it's a file and line number)
    local cursor_word = vim.fn.expand("<cfile>")
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

    return nil -- Return nil if no client with the specified name is found
end

M.strip_ansi = function(s)
    if not s then
        return s
    end

    -- Remove ANSI CSI sequences: ESC [ digits ; digits ... letter
    return s:gsub("\27%[[0-9;]*[A-Za-z]", "")
end

M.is_file_exists = function(filepath)
    return vim.fn.filereadable(filepath) == 1
end

M.close_window_if_exists = function(win_id)
    if win_id and vim.api.nvim_win_is_valid(win_id) then
        vim.api.nvim_win_close(win_id, true)
    end
end

-- M.table_to_string = function(tbl, indent)
--     indent = indent or 0
--     local to_log = string.rep(" ", indent) .. "{\n"
--
--     for k, v in pairs(tbl) do
--         local key = tostring(k)
--         if type(v) == "table" then
--             to_log = to_log .. string.rep(" ", indent + 2) .. key .. " = " .. M.table_to_string(v, indent + 2) .. ",\n"
--         else
--             local value = tostring(v)
--             to_log = to_log .. string.rep(" ", indent + 2) .. key .. " = " .. value .. ",\n"
--         end
--     end
--
--     to_log = to_log .. string.rep(" ", indent) .. "}"
--     return to_log
-- end
--
-- M.log_table = function(table)
--     local str_table = M.table_to_string(table)
--     LazyVim.info("lua talbe: " .. str_table)
-- end

--lua print(vim.inspect(vim.lsp.get_active_clients()[1].name))

return M
