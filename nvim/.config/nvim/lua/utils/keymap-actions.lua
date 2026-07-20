-- Global keymap callbacks kept out of lua/config/keymaps.lua.
--
-- • open_personal_vim_cheat_sheet — open the personal cheat sheet split
-- • open_cwd_scratch_notes — open the project scratch note
-- • copy_* — copy current file paths/names to the system clipboard
-- • search_clipboard — start a slash search from the clipboard
-- • convert_* / normalize_* — visual JSON conversion helpers

local M = {}

--- Return the current buffer path, or notify when the buffer has no file.
---@return string|nil
local function current_buffer_path()
    local absolute_path = vim.api.nvim_buf_get_name(0)
    if absolute_path == "" then
        vim.notify("No file path for current buffer", vim.log.levels.WARN)
        return nil
    end

    return absolute_path
end

--- Copy text to the system clipboard and show a notification.
---@param text string
local function copy_to_clipboard(text)
    vim.fn.setreg("+", text)
    vim.notify("Copied: " .. text, vim.log.levels.INFO)
end

--- Exit visual mode so visual selection marks are available to helper modules.
local function exit_visual_mode()
    local esc = vim.api.nvim_replace_termcodes("<Esc>", true, false, true)
    vim.api.nvim_feedkeys(esc, "x", false)
end

--- Return the current visual selection as a line/column range string.
---@return string
local function visual_selection_position()
    local start_pos = vim.fn.getpos("'<")
    local end_pos = vim.fn.getpos("'>")
    local start_line = start_pos[2]
    local start_col = start_pos[3]
    local end_line = end_pos[2]
    local end_col = end_pos[3]

    if start_line > end_line or (start_line == end_line and start_col > end_col) then
        start_line, end_line = end_line, start_line
        start_col, end_col = end_col, start_col
    end

    if end_col >= 2147483647 then
        local end_line_text = vim.api.nvim_buf_get_lines(0, end_line - 1, end_line, false)[1] or ""
        end_col = #end_line_text
    end

    if start_line == end_line then
        return ("%d:%d-%d"):format(start_line, start_col, end_col)
    end

    return ("%d:%d-%d:%d"):format(start_line, start_col, end_line, end_col)
end

--- Add a buffer-local close keymap for the opened cheat sheet.
---@param bufnr integer
local function configure_cheat_sheet_close_key(bufnr)
    vim.keymap.set("n", "q", ("<cmd>bdelete! %d<cr>"):format(bufnr), { buffer = bufnr, desc = "Close" })
end

--- Open the personal Vim cheat sheet in a split.
function M.open_personal_vim_cheat_sheet()
    local path = vim.fn.glob("$HOME/.config/nvim/cheat_sheet/vim_cheat_sheet.md")
    if path == "" then
        vim.notify("Personal Vim cheat sheet not found", vim.log.levels.WARN)
        return
    end

    vim.cmd("sp " .. vim.fn.fnameescape(path))
    configure_cheat_sheet_close_key(vim.api.nvim_get_current_buf())
end

--- Open the current working directory scratch note.
function M.open_cwd_scratch_notes()
    Snacks.scratch({ name = "CWD Notes", ft = "txt" })
end

--- Copy the current buffer file path with a fnamemodify modifier.
---@param path_modifier string
function M.copy_file_path(path_modifier)
    local absolute_path = current_buffer_path()
    if not absolute_path then
        return
    end

    copy_to_clipboard(vim.fn.fnamemodify(absolute_path, path_modifier))
end

--- Copy the current buffer path relative to the working directory.
function M.copy_relative_file_path()
    M.copy_file_path(":.")
end

--- Copy the current buffer absolute path.
function M.copy_absolute_file_path()
    M.copy_file_path(":p")
end

--- Copy the current buffer absolute file path with cursor position or visual range.
---@param use_visual_selection boolean|nil
function M.copy_absolute_file_path_with_position(use_visual_selection)
    local absolute_path = current_buffer_path()
    if not absolute_path then
        return
    end

    local path = vim.fn.fnamemodify(absolute_path, ":p")
    local path_with_position
    if use_visual_selection then
        exit_visual_mode()
        path_with_position = ("%s:%s"):format(path, visual_selection_position())
    else
        local cursor = vim.api.nvim_win_get_cursor(0)
        path_with_position = ("%s:%d:%d"):format(path, cursor[1], cursor[2] + 1)
    end

    copy_to_clipboard(path_with_position)
end

--- Copy the current file name with a vim expand modifier.
---@param path_modifier string
function M.copy_current_file_name(path_modifier)
    local name = vim.fn.expand(path_modifier)
    if name == "" then
        vim.notify("No file name for current buffer", vim.log.levels.WARN)
        return
    end

    copy_to_clipboard(name)
end

--- Copy the current file name without its extension.
function M.copy_file_name_without_ext()
    M.copy_current_file_name("%:t:r")
end

--- Copy the current file name with its extension.
function M.copy_file_name()
    M.copy_current_file_name("%:t")
end

--- Start a slash search using the clipboard contents.
function M.search_clipboard()
    local text = vim.fn.escape((vim.fn.getreg("+"):gsub("\n", "")), "/\\")
    vim.fn.feedkeys("/" .. text, "n")
end

--- Convert the selected Java toString() text to JSON in place.
function M.convert_java_tostring_to_json_replace()
    exit_visual_mode()
    require("utils.java.java-tostring-parser").convert_selection(false)
end

--- Convert the selected Java toString() text to JSON and copy it.
function M.convert_java_tostring_to_json_clipboard()
    exit_visual_mode()
    require("utils.java.java-tostring-parser").convert_selection(true)
end

--- Normalize JSON in the current buffer.
function M.normalize_json_buffer()
    require("utils.json.normalize").normalize_buffer()
end

--- Normalize JSON in the current visual selection.
function M.normalize_json_selection()
    exit_visual_mode()
    require("utils.json.normalize").normalize_selection()
end

return M
