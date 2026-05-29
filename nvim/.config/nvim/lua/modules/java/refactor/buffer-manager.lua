-- Buffer management for Java refactoring.
-- Tracks opened buffers before file moves, deletes old buffers,
-- and reopens them at new locations after refactoring completes.

local M = {}

local buffer_util = require("utils.buffer-util")
local consts = require("modules.java.refactor.constants")

local log = consts.log
local shell_escape = consts.shell_escape

---@class BufferReopenInfo
---@field old_path string
---@field new_path string
---@field buf_id number
---@field win_id? number
---@field is_current? boolean
---@field scratch_buf? number

--- Track all opened buffers that will be affected by file moves.
--- Scans both directory moves (finds all files under them) and individual file moves.
---@param all_changes java.rejactor.FileMove[]
---@return BufferReopenInfo[]
function M.track_buffers(all_changes)
    local opened_buffers = {}
    log.info("Tracking opened buffers before applying changes...")

    for _, change in ipairs(all_changes) do
        if vim.fn.isdirectory(change.src) == 1 then
            log.debug("Scanning directory for open buffers:", change.src)
            local handle = io.popen("fd -e java . " .. shell_escape(change.src))
            if handle then
                local file_count = 0
                for file_path in handle:lines() do
                    file_count = file_count + 1
                    log.debug("Checking file:", file_path)
                    local buf_id = buffer_util.find_buf_by_path(file_path)
                    if buf_id then
                        local new_path = file_path:gsub("^" .. vim.pesc(change.src), change.dst)
                        table.insert(opened_buffers, {
                            old_path = file_path,
                            new_path = new_path,
                            buf_id = buf_id,
                        })
                        log.info("Will reopen buffer:", file_path, "->", new_path)
                    else
                        log.debug("File not open in buffer:", file_path)
                    end
                end
                handle:close()
                log.debug("Scanned", file_count, "files in", change.src)
            end
        elseif change.src:match("%.java$") then
            log.debug("Checking single file:", change.src)
            local buf_id = buffer_util.find_buf_by_path(change.src)
            if buf_id then
                table.insert(opened_buffers, {
                    old_path = change.src,
                    new_path = change.dst,
                    buf_id = buf_id,
                })
                log.info("Will reopen buffer:", change.src, "->", change.dst)
            else
                log.debug("File not open in buffer:", change.src)
            end
        end
    end

    if #opened_buffers > 0 then
        log.info("Found", #opened_buffers, "opened buffers to reopen after changes")
    else
        log.info("No opened buffers found for files being moved")
    end

    return opened_buffers
end

--- Delete old buffers before applying changes.
--- For the current buffer: switches to a scratch buffer first, then deletes.
--- For other buffers: just deletes them.
---@param opened_buffers BufferReopenInfo[]
function M.delete_old_buffers(opened_buffers)
    if #opened_buffers == 0 then
        return
    end

    log.info("Processing", #opened_buffers, "old buffers before applying changes...")

    local current_win = vim.api.nvim_get_current_win()
    local current_buf = vim.api.nvim_get_current_buf()
    log.debug("Current window:", current_win, "Current buffer:", current_buf)

    for i, buf_info in ipairs(opened_buffers) do
        log.debug("Processing buffer", i, "of", #opened_buffers)
        if vim.api.nvim_buf_is_valid(buf_info.buf_id) then
            -- Store window ID BEFORE deleting buffer
            buf_info.win_id = vim.fn.bufwinid(buf_info.buf_id)
            buf_info.is_current = (buf_info.buf_id == current_buf)
            log.debug("Buffer", buf_info.buf_id, "in window", buf_info.win_id, "is_current:", buf_info.is_current)

            if buf_info.is_current then
                -- For the CURRENT buffer: switch to a scratch buffer first
                local scratch = vim.api.nvim_create_buf(false, true)
                vim.api.nvim_win_set_buf(current_win, scratch)
                local success, err = pcall(vim.api.nvim_buf_delete, buf_info.buf_id, { force = true })
                if success then
                    log.info("Switched current window away from moved buffer:", buf_info.old_path)
                else
                    log.warn("Failed to delete current buffer:", buf_info.old_path, "Error:", err)
                end
                buf_info.scratch_buf = scratch
            else
                -- For non-current buffers: just delete
                local success, err = pcall(vim.api.nvim_buf_delete, buf_info.buf_id, { force = true })
                if success then
                    log.info("Deleted old buffer:", buf_info.old_path, "(was in window", buf_info.win_id, ")")
                else
                    log.warn("Failed to delete buffer:", buf_info.old_path, "Error:", err)
                end
            end
        else
            log.debug("Buffer already invalid:", buf_info.old_path)
        end
    end
end

--- Reopen buffers from their new locations after refactoring.
---@param opened_buffers BufferReopenInfo[]
function M.reopen_buffers(opened_buffers)
    if #opened_buffers == 0 then
        return
    end

    log.info("Reopening", #opened_buffers, "buffers from new locations...")

    for i, buf_info in ipairs(opened_buffers) do
        log.debug("Reopening buffer", i, "of", #opened_buffers, ":", buf_info.new_path)
        if vim.fn.filereadable(buf_info.new_path) == 1 then
            log.debug("File exists at new location:", buf_info.new_path)

            if buf_info.is_current then
                -- This was the focused buffer — open in the current window
                local target_win = vim.api.nvim_get_current_win()
                if buf_info.win_id and buf_info.win_id ~= -1 and vim.api.nvim_win_is_valid(buf_info.win_id) then
                    target_win = buf_info.win_id
                end
                vim.api.nvim_win_call(target_win, function()
                    vim.cmd("edit " .. vim.fn.fnameescape(buf_info.new_path))
                    vim.cmd("filetype detect")
                end)
                -- Clean up scratch buffer
                if buf_info.scratch_buf and vim.api.nvim_buf_is_valid(buf_info.scratch_buf) then
                    pcall(vim.api.nvim_buf_delete, buf_info.scratch_buf, { force = true })
                end
                log.info("Switched current window to new file:", buf_info.new_path)
            elseif buf_info.win_id and buf_info.win_id ~= -1 and vim.api.nvim_win_is_valid(buf_info.win_id) then
                -- Buffer was displayed in a window, open new file in that window
                log.debug("Reopening in window", buf_info.win_id)
                vim.api.nvim_win_call(buf_info.win_id, function()
                    vim.cmd("edit " .. vim.fn.fnameescape(buf_info.new_path))
                    vim.cmd("filetype detect")
                end)
                log.info("Reopened buffer in window", buf_info.win_id, ":", buf_info.new_path)
            else
                -- Buffer was not displayed or window no longer valid, just load it
                log.debug("Loading as hidden buffer (win_id:", buf_info.win_id, ")")
                vim.cmd("badd " .. vim.fn.fnameescape(buf_info.new_path))
                log.info("Loaded hidden buffer:", buf_info.new_path)
            end
        else
            log.warn("New file not found, cannot reopen:", buf_info.new_path)
        end
    end
    log.info("Buffer reopening completed")
end

return M
