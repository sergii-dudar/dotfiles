local M = {}

-- get loaded buffers, same as by `:ls` command or telescope buffers
function M.get_active_ls_buffers()
    local buffers = vim.api.nvim_list_bufs()
    local active_buffers = {}

    for _, buf in ipairs(buffers) do
        if vim.api.nvim_buf_is_loaded(buf) and vim.api.nvim_buf_get_option(buf, "buflisted") then
            table.insert(active_buffers, buf)
        end
    end

    return active_buffers
end

return M