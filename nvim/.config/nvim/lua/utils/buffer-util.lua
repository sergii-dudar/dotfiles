local M = {}

local work_buffer_types = {
    java = true,
    lua = true,
}

-- get loaded buffers, same as by `:ls` command or telescope buffers
M.get_active_ls_buffers = function()
    local buffers = vim.api.nvim_list_bufs()
    local active_buffers = {}
    for _, buf in ipairs(buffers) do
        local buff_file_type = vim.api.nvim_buf_get_option(buf, "filetype")
        if
            vim.api.nvim_buf_is_loaded(buf)
            and (vim.api.nvim_buf_get_option(buf, "buflisted") or work_buffer_types[buff_file_type])
        then
            table.insert(active_buffers, buf)
        end
    end

    return active_buffers
end

return M
