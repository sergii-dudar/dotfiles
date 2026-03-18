local M = {}

local shown = false
local snapshot_lines = nil
local log_bufnr = nil

---Snapshot the dapui console buffer content.
---Call this BEFORE dapui.close() to preserve the output.
function M.snapshot()
    local ok_dapui, dapui = pcall(require, "dapui")
    if not ok_dapui then
        return
    end

    local console_buf = dapui.elements.console.buffer()
    if not console_buf or not vim.api.nvim_buf_is_valid(console_buf) then
        return
    end

    local lines = vim.api.nvim_buf_get_lines(console_buf, 0, -1, false)
    if #lines == 0 or (#lines == 1 and lines[1] == "") then
        snapshot_lines = nil
        return
    end

    snapshot_lines = lines
end

---Show the snapshotted DAP logs in a bottom split.
---Call this after dapui is closed.
function M.show_logs()
    -- Guard against duplicate calls (event_terminated + event_exited)
    if shown or not snapshot_lines then
        return
    end

    shown = true

    log_bufnr = vim.api.nvim_create_buf(false, true)
    vim.api.nvim_buf_set_lines(log_bufnr, 0, -1, false, snapshot_lines)
    -- snapshot_lines = nil

    require("utils.buffer-util").open_scratch_split(log_bufnr)
    vim.cmd("normal! G")
end

---Close the log split if open, and reset state for a new debug session.
function M.reset()
    if log_bufnr and vim.api.nvim_buf_is_valid(log_bufnr) then
        for _, win in ipairs(vim.api.nvim_list_wins()) do
            if vim.api.nvim_win_get_buf(win) == log_bufnr then
                vim.api.nvim_win_close(win, true)
            end
        end
    end
    log_bufnr = nil
    shown = false
    snapshot_lines = nil
end

return M
