-- Global keymap callbacks kept out of lua/config/keymaps.lua.
--
-- • open_personal_vim_cheat_sheet — open the personal cheat sheet split
-- • open_cwd_scratch_notes — open the project scratch note
-- • copy_* — copy current file paths/names (optionally with cursor position or visual range) to the system clipboard
-- • copy_selection_with_reference — copy the visual selection as "relative/path:range" + fenced code for AI prompts
-- • search_clipboard — start a slash search from the clipboard
-- • convert_* / normalize_* — visual JSON conversion helpers
-- • decode_base64_or_jwt_selection — decode the selected base64 / JWT into a scratch split

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
---@param summary string|nil shown in the notification instead of the full text
local function copy_to_clipboard(text, summary)
    vim.fn.setreg("+", text)
    vim.notify("Copied: " .. (summary or text), vim.log.levels.INFO)
end

--- Leave visual mode once the selection has been read, like `y` does.
local function exit_visual_mode()
    local esc = vim.api.nvim_replace_termcodes("<Esc>", true, false, true)
    vim.api.nvim_feedkeys(esc, "nx", false)
end

---@class VisualSelection
---@field mode string "v" (charwise), "V" (linewise) or "\22" (blockwise)
---@field start_line integer
---@field start_col integer 1-based byte column
---@field end_line integer
---@field end_col integer 1-based byte column, clamped to the end line length
---@field lines string[] selected text, one entry per line

--- Return the active visual selection, or the last one when called from normal mode.
---@return VisualSelection
local function visual_selection()
    local mode = vim.fn.mode()
    local active = mode == "v" or mode == "V" or mode == "\22"
    if not active then
        mode = vim.fn.visualmode()
        if mode == "" then
            mode = "V"
        end
    end
    local pos1 = vim.fn.getpos(active and "v" or "'<")
    local pos2 = vim.fn.getpos(active and "." or "'>")

    local start_line, start_col, end_line, end_col = pos1[2], pos1[3], pos2[2], pos2[3]
    if mode == "\22" then
        -- the cursor can sit on any corner of the block
        start_line, end_line = math.min(start_line, end_line), math.max(start_line, end_line)
        start_col, end_col = math.min(start_col, end_col), math.max(start_col, end_col)
    elseif start_line > end_line or (start_line == end_line and start_col > end_col) then
        start_line, end_line = end_line, start_line
        start_col, end_col = end_col, start_col
    end
    if mode == "V" then
        start_col = 1
    end
    -- linewise marks report v:maxcol and an empty line reports 0: clamp to a real column
    end_col = math.max(1, math.min(end_col, #vim.fn.getline(end_line)))

    return {
        mode = mode,
        start_line = start_line,
        start_col = start_col,
        end_line = end_line,
        end_col = end_col,
        lines = vim.fn.getregion(pos1, pos2, { type = mode }),
    }
end

--- Format a selection as "L1-L2" for whole lines, otherwise "L:C1-C2" or "L1:C1-L2:C2".
---@param sel VisualSelection
---@return string
local function selection_position(sel)
    if sel.mode == "V" then
        if sel.start_line == sel.end_line then
            return tostring(sel.start_line)
        end
        return ("%d-%d"):format(sel.start_line, sel.end_line)
    end
    if sel.start_line == sel.end_line then
        return ("%d:%d-%d"):format(sel.start_line, sel.start_col, sel.end_col)
    end
    return ("%d:%d-%d:%d"):format(sel.start_line, sel.start_col, sel.end_line, sel.end_col)
end

--- Strip the leading whitespace shared by every non-blank line; blank lines become empty.
---@param lines string[]
---@return string[]
local function dedent(lines)
    local common
    for _, line in ipairs(lines) do
        if line:find("%S") then
            local indent = line:match("^%s*")
            if common == nil then
                common = indent
            else
                local n = 0
                while n < #common and n < #indent and common:byte(n + 1) == indent:byte(n + 1) do
                    n = n + 1
                end
                common = common:sub(1, n)
            end
            if common == "" then
                break
            end
        end
    end
    if not common or common == "" then
        return lines
    end
    return vim.tbl_map(function(line)
        return line:find("%S") and line:sub(#common + 1) or ""
    end, lines)
end

--- Return a markdown code fence longer than any backtick run inside the text.
---@param text string
---@return string
local function code_fence(text)
    local longest = 2
    for run in text:gmatch("`+") do
        longest = math.max(longest, #run)
    end
    return ("`"):rep(longest + 1)
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

--- Copy the current buffer path (after a fnamemodify modifier) with the cursor position or visual range.
---@param path_modifier string
---@param use_visual_selection boolean|nil
function M.copy_file_path_with_position(path_modifier, use_visual_selection)
    local absolute_path = current_buffer_path()
    if not absolute_path then
        return
    end

    local path = vim.fn.fnamemodify(absolute_path, path_modifier)
    local position
    if use_visual_selection then
        position = selection_position(visual_selection())
        exit_visual_mode()
    else
        local cursor = vim.api.nvim_win_get_cursor(0)
        position = ("%d:%d"):format(cursor[1], cursor[2] + 1)
    end

    copy_to_clipboard(("%s:%s"):format(path, position))
end

--- Copy the current buffer absolute file path with cursor position or visual range.
---@param use_visual_selection boolean|nil
function M.copy_absolute_file_path_with_position(use_visual_selection)
    M.copy_file_path_with_position(":p", use_visual_selection)
end

--- Copy the current buffer path relative to the working directory with cursor position or visual range.
---@param use_visual_selection boolean|nil
function M.copy_relative_file_path_with_position(use_visual_selection)
    M.copy_file_path_with_position(":.", use_visual_selection)
end

--- Copy the visual selection for an AI prompt: the relative path with the selected range, then the
--- text in a fenced code block tagged with the buffer filetype and stripped of its common indent.
function M.copy_selection_with_reference()
    local absolute_path = current_buffer_path()
    if not absolute_path then
        return
    end

    local sel = visual_selection()
    exit_visual_mode()
    local reference = ("%s:%s"):format(vim.fn.fnamemodify(absolute_path, ":."), selection_position(sel))
    local code = table.concat(dedent(sel.lines), "\n")
    local fence = code_fence(code)
    local text = table.concat({ reference, fence .. vim.bo.filetype, code, fence }, "\n")

    copy_to_clipboard(text, ("%s (%d lines)"):format(reference, #sel.lines))
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

--- Decode the selected base64 or JWT text into a scratch split below (`q` closes it).
function M.decode_base64_or_jwt_selection()
    exit_visual_mode()
    require("utils.decode-util").decode_selection()
end

return M
