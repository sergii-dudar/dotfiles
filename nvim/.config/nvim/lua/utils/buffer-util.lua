local M = {}

local work_buffer_types = {
    java = true,
    lua = true,
}

-- get loaded bufferIds list, same as by `:ls` command or telescope buffers
function M.get_active_ls_buffers()
    local buffers = vim.api.nvim_list_bufs()
    local active_buffers = {}
    for _, buf in ipairs(buffers) do
        -- local buff_file_type = vim.api.nvim_buf_get_option(buf, "filetype")
        local buff_file_type = vim.api.nvim_get_option_value("filetype", { buf = buf })
        if
            vim.api.nvim_buf_is_loaded(buf)
            -- and (vim.api.nvim_buf_get_option(buf, "buflisted") or work_buffer_types[buff_file_type])
            and (vim.api.nvim_get_option_value("buflisted", { buf = buf }) or work_buffer_types[buff_file_type])
        then
            table.insert(active_buffers, buf)
        end
    end

    return active_buffers
end

---@return integer|nil
function M.find_buf_by_path(path)
    path = vim.fn.fnamemodify(path, ":p")
    for _, buf in ipairs(vim.api.nvim_list_bufs()) do
        if vim.api.nvim_buf_is_loaded(buf) then
            local name = vim.api.nvim_buf_get_name(buf)
            if name ~= "" and vim.fn.fnamemodify(name, ":p") == path then
                return buf
            end
        end
    end

    return nil
end

function M.close_buffer_by_path(path)
    local buf = M.find_buf_by_path(path)
    if buf then
        vim.api.nvim_buf_delete(buf, { force = false })
        return true
    end
    return false
end

function M.open_buffer_by_path(path)
    vim.cmd.edit(path)
end

return M
