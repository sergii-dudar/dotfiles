local M = {}
local data = {}

function M.save_position()
    data.prev_win = vim.api.nvim_get_current_win()
    data.prev_view = vim.fn.winsaveview()
end

function M.restore_position()
    if data.prev_win and data.prev_view and vim.api.nvim_win_is_valid(data.prev_win) then
        vim.api.nvim_set_current_win(data.prev_win)
        vim.fn.winrestview(data.prev_view)
        data.prev_win = nil
        data.prev_view = nil
    end
end

return M
