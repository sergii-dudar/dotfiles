-- Window position management and split helpers.
--
-- • save_position — save current window and cursor view
-- • preserve_cursor — restore saved cursor in original window
-- • restore_position — focus saved window and restore view
-- • bot_split — open a bottom split with given height
-- • overseer_open — open overseer panel at bottom

local M = {}
local data = {}

--- Save the current window and cursor view.
function M.save_position()
    data.prev_win = vim.api.nvim_get_current_win()
    data.prev_view = vim.fn.winsaveview()
end

--- Restore the cursor view in the saved window.
function M.preserve_cursor()
    if data.prev_win and data.prev_view then
        vim.api.nvim_win_call(data.prev_win, function()
            vim.fn.winrestview(data.prev_view)
        end)
    end
end

--- Restore the saved window and cursor view.
function M.restore_position()
    if data.prev_win and data.prev_view and vim.api.nvim_win_is_valid(data.prev_win) then
        vim.api.nvim_set_current_win(data.prev_win)
        vim.fn.winrestview(data.prev_view)
        data.prev_win = nil
        data.prev_view = nil
    end
end

--- Open a bottom split and preserve the previous cursor position.
function M.bot_split()
    -- if not data.prev_win or not data.prev_view then
    M.save_position()
    -- end
    vim.cmd("botright split")
    M.preserve_cursor()
end

---@param opts nil|overseer.WindowOpts
function M.overseer_open(opts)
    M.save_position()
    require("overseer").open(opts)
    M.preserve_cursor()
end

return M
