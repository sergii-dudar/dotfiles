-- Buffer management for Java refactoring.
-- Tracks opened buffers before file moves, deletes old buffers,
-- and reopens them at new locations after refactoring completes.

local M = {}

local buffer_util = require("utils.buffer-util")
local consts = require("modules.java.refactor.constants")

local log = consts.log
local shell_escape = consts.shell_escape

local function normalize_path(path)
    if not path or path == "" then
        return nil
    end
    return vim.fn.fnamemodify(path, ":p"):gsub("/$", "")
end

local function add_tracked_buffer(opened_buffers, seen, old_path, new_path, buf_id)
    local key = tostring(buf_id) .. "\n" .. new_path
    if seen[key] then
        return false
    end

    seen[key] = true
    table.insert(opened_buffers, {
        old_path = old_path,
        new_path = new_path,
        buf_id = buf_id,
    })
    log.info("Will reopen buffer:", old_path, "->", new_path)
    return true
end

---@class BufferReopenInfo
---@field old_path string
---@field new_path string
---@field buf_id number
---@field win_id? number
---@field is_current? boolean
---@field scratch_buf? number

--- Persist modified moved buffers to their destination paths before buffer cleanup.
---@param opened_buffers BufferReopenInfo[]
---@return boolean success
function M.write_modified_buffers(opened_buffers)
    if #opened_buffers == 0 then
        return true
    end

    for _, buf_info in ipairs(opened_buffers) do
        if
            vim.api.nvim_buf_is_valid(buf_info.buf_id)
            and vim.api.nvim_get_option_value("modified", { buf = buf_info.buf_id })
        then
            log.warn("Buffer has unsaved changes, writing to moved path before cleanup:", buf_info.new_path)

            local parent_dir = buf_info.new_path:match("(.+)/[^/]+$")
            if parent_dir then
                vim.fn.mkdir(parent_dir, "p")
            end

            local ok, err = pcall(vim.api.nvim_buf_call, buf_info.buf_id, function()
                vim.cmd("silent keepalt write! " .. vim.fn.fnameescape(buf_info.new_path))
                vim.api.nvim_set_option_value("modified", false, { buf = buf_info.buf_id })
            end)

            if not ok then
                log.error(
                    "Failed to write modified buffer before refactor:",
                    buf_info.old_path,
                    "->",
                    buf_info.new_path,
                    err
                )
                vim.notify(
                    "Java refactor aborted: failed to save modified buffer to " .. buf_info.new_path,
                    vim.log.levels.ERROR
                )
                return false
            end
        end
    end

    return true
end

--- Append opened buffers affected by file moves to an existing tracking list.
--- Scans loaded buffers first so tracking still works after broad package moves.
---@param all_changes java.rejactor.FileMove[]
---@param opened_buffers BufferReopenInfo[]
---@param seen? table<string, boolean>
---@return table<string, boolean> seen
function M.track_buffers_into(all_changes, opened_buffers, seen)
    seen = seen or {}

    for _, change in ipairs(all_changes) do
        local src = normalize_path(change.src)
        local dst = normalize_path(change.dst)

        if not src or not dst then
            goto continue_change
        end

        local is_dir_move = vim.fn.isdirectory(src) == 1 or not src:match("%.java$")
        local matched_loaded_buffer = false

        for _, buf_id in ipairs(vim.api.nvim_list_bufs()) do
            if vim.api.nvim_buf_is_loaded(buf_id) then
                local buf_path = normalize_path(vim.api.nvim_buf_get_name(buf_id))
                if buf_path then
                    local new_path = nil
                    if is_dir_move and (buf_path == src or buf_path:find("^" .. vim.pesc(src) .. "/")) then
                        new_path = buf_path:gsub("^" .. vim.pesc(src), dst)
                    elseif not is_dir_move and buf_path == src then
                        new_path = dst
                    end

                    if new_path then
                        matched_loaded_buffer = add_tracked_buffer(opened_buffers, seen, buf_path, new_path, buf_id)
                            or matched_loaded_buffer
                    end
                end
            end
        end

        if vim.fn.isdirectory(src) == 1 then
            log.debug("Scanning directory for open buffers:", change.src)
            local handle = io.popen("fd -e java . " .. shell_escape(src))
            if handle then
                local file_count = 0
                for file_path in handle:lines() do
                    file_count = file_count + 1
                    log.debug("Checking file:", file_path)
                    local buf_id = buffer_util.find_buf_by_path(file_path)
                    if buf_id then
                        local normalized_file_path = normalize_path(file_path) or file_path
                        local new_path = normalized_file_path:gsub("^" .. vim.pesc(src), dst)
                        add_tracked_buffer(opened_buffers, seen, normalized_file_path, new_path, buf_id)
                    else
                        log.debug("File not open in buffer:", file_path)
                    end
                end
                handle:close()
                log.debug("Scanned", file_count, "files in", change.src)
            end
        elseif change.src:match("%.java$") and not matched_loaded_buffer then
            log.debug("Checking single file:", change.src)
            local buf_id = buffer_util.find_buf_by_path(src)
            if buf_id then
                add_tracked_buffer(opened_buffers, seen, src, dst, buf_id)
            else
                log.debug("File not open in buffer:", change.src)
            end
        end

        ::continue_change::
    end

    return seen
end

--- Track all opened buffers that will be affected by file moves.
--- Scans both loaded buffers and source directories.
---@param all_changes java.rejactor.FileMove[]
---@return BufferReopenInfo[]
function M.track_buffers(all_changes)
    local opened_buffers = {}
    log.info("Tracking opened buffers before applying changes...")

    M.track_buffers_into(all_changes, opened_buffers)

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
                local success, err = pcall(vim.api.nvim_buf_delete, buf_info.buf_id, { force = false })
                if success then
                    log.info("Switched current window away from moved buffer:", buf_info.old_path)
                else
                    log.warn("Failed to delete current buffer:", buf_info.old_path, "Error:", err)
                end
                buf_info.scratch_buf = scratch
            else
                -- For non-current buffers: just delete
                local success, err = pcall(vim.api.nvim_buf_delete, buf_info.buf_id, { force = false })
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
