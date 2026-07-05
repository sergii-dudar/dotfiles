local M = {}

---Re-assert a picker's jump position across a short settle window.
---
---Some LSP navigation targets open in buffers whose content arrives AFTER the window is
---shown: `jdt://` decompiled Java (nvim-jdtls `BufReadCmd` -> `java/classFileContents`),
---and equivalents in other languages/LSPs. When Snacks confirms such a result it opens
---the buffer and immediately calls `nvim_win_set_cursor{lnum,col}`; if the line isn't
---there yet the call is out of range ("Invalid cursor line: out of range") and the window
---is left on line 1, with the source arriving a beat later. The same end state also shows
---up on an already-loaded virtual buffer (the position gets clobbered after Snacks set
---it). Ordinary on-disk files load synchronously and are unaffected.
---
---Rather than special-casing a scheme, this keeps the cursor honest by polling the
---jumped-to window:
---  • while the buffer is still shorter than `lnum` (source not in yet), it waits;
---  • once the buffer holds the line but the cursor sits on line 1 — the signature of
---    both the cold race (the out-of-range set left us at the top) and a late content
---    replace (`nvim_buf_set_lines(0,-1)` snapping the cursor to the top) — it sets the
---    cursor to the intended `item.pos` (column clamped to the line) and centers;
---  • if the cursor is on any OTHER line, that is a deliberate move, so it bails rather
---    than yank the user back.
---For an ordinary on-disk file the cursor is already correct on the first tick, so it
---stops immediately and never moves anything. For a virtual buffer (`buftype ~= ""`, e.g.
---`jdt://`) it keeps a short watch so a late async replace still gets corrected. Bounded
---and self-terminating, so it is safe to run for every confirm, in any language / project.
---@param item snacks.picker.finder.Item|nil the item the picker navigated to
function M.reposition_after_jump(item)
    if not (item and item.pos and item.pos[1] and item.pos[1] >= 1) then
        return
    end
    local lnum = item.pos[1]
    local col = item.pos[2] or 0

    local max_attempts = 30 -- ceiling ~ 30 * 40ms = 1.2s (well within jdt_uri_timeout_ms)
    local interval_ms = 40

    -- The buffer the jump opened for this item. Resolve by the same normalized path Snacks
    -- used to `bufadd` it, falling back to the raw `item.file`. We locate the *target* by
    -- buffer — NOT by whatever window happens to be focused when our timer fires: right
    -- after a jump focus can momentarily sit on the explorer/other window, and pinning that
    -- would make us abandon the real target. `bufnr()` does prefix matching, so confirm the
    -- resolved buffer's name actually matches before trusting it.
    local want = (Snacks and Snacks.picker and Snacks.picker.util.path(item)) or item.file
    if not want then
        return
    end

    ---Find a normal (non-floating) window currently displaying the target buffer.
    ---@return integer? win, integer? buf
    local function locate()
        local buf = vim.fn.bufnr(want)
        if buf == -1 then
            buf = vim.fn.bufnr(item.file)
        end
        if buf == -1 or not vim.api.nvim_buf_is_valid(buf) then
            return nil, nil
        end
        local name = vim.api.nvim_buf_get_name(buf)
        if name ~= want and name ~= item.file then
            return nil, nil -- prefix-matched a different buffer; ignore
        end
        for _, win in ipairs(vim.fn.win_findbuf(buf)) do
            if vim.api.nvim_win_is_valid(win) and vim.api.nvim_win_get_config(win).relative == "" then
                return win, buf
            end
        end
        return nil, buf
    end

    local function set_cursor(win, buf)
        local line = vim.api.nvim_buf_get_lines(buf, lnum - 1, lnum, false)[1] or ""
        pcall(vim.api.nvim_win_set_cursor, win, { lnum, math.min(col, #line) })
        vim.api.nvim_win_call(win, function()
            vim.cmd("normal! zzzv")
        end)
    end

    local function attempt(n)
        local win, buf = locate()
        if win and buf and vim.api.nvim_buf_line_count(buf) >= lnum then
            local cur = vim.api.nvim_win_get_cursor(win)[1]
            if cur == lnum then
                -- Correctly placed. On-disk files can't be clobbered afterwards, so stop;
                -- virtual buffers keep the watch (a late replace can still snap the cursor
                -- to the top).
                if vim.bo[buf].buftype == "" then
                    return
                end
            elseif cur == 1 then
                -- Cold race or late content replace parked us at the top: correct it.
                set_cursor(win, buf)
            else
                -- Cursor is on some other line: a deliberate move. Leave it be.
                return
            end
        end

        if n >= max_attempts then
            return
        end
        vim.defer_fn(function()
            attempt(n + 1)
        end, interval_ms)
    end

    -- Scheduled so it runs after Snacks' own jump (which may itself defer when leaving
    -- insert mode).
    vim.schedule(function()
        attempt(1)
    end)
end

---Snacks picker `confirm` action: run the builtin jump, then keep the cursor on the
---intended position (see `reposition_after_jump`). Drop-in `confirm` override for LSP
---navigation pickers and our MapStruct `gr` picker; language-agnostic. Captures the
---navigated item before jumping (the picker may close during the jump).
---@param picker snacks.Picker
---@param item snacks.picker.finder.Item
---@param action snacks.picker.Action
function M.confirm_with_reposition(picker, item, action)
    local ok, selected = pcall(function()
        return picker:selected({ fallback = true })
    end)
    local target = (ok and selected and selected[1]) or item
    Snacks.picker.actions.jump(picker, item, action)
    M.reposition_after_jump(target)
end

---Close the active picker and schedule another picker to open.
---@param picker snacks.Picker
---@param open fun()
local function switch_picker(picker, open)
    picker:norm(function()
        picker:close()
        vim.schedule(open)
    end)
end

---Resolve the buffer that should be treated as current for picker filtering.
---@param picker? snacks.Picker
---@return number
local function picker_current_buf(picker)
    local filter_buf = picker and picker.input and picker.input.filter and picker.input.filter.current_buf
    if filter_buf and vim.api.nvim_buf_is_valid(filter_buf) then
        return filter_buf
    end

    if picker then
        local ok, main_buf = pcall(vim.api.nvim_win_get_buf, picker.main)
        if ok and vim.api.nvim_buf_is_valid(main_buf) then
            return main_buf
        end
    end

    return vim.api.nvim_get_current_buf()
end

---Resolve the current working directory from the active picker context.
---@param picker? snacks.Picker
---@return string
local function picker_cwd(picker)
    local filter_cwd = picker and picker.input and picker.input.filter and picker.input.filter.cwd
    if filter_cwd then
        return filter_cwd
    end

    if picker then
        local ok, cwd = pcall(function()
            return picker:cwd()
        end)
        if ok and cwd then
            return cwd
        end
    end

    return vim.fn.getcwd(0)
end

---Read the visible input text from the active picker.
---@param picker snacks.Picker
---@return string
local function picker_input_text(picker)
    local ok, input = pcall(function()
        return picker.input and picker.input:get()
    end)
    if ok and type(input) == "string" then
        return input
    end

    local filter = picker.input and picker.input.filter
    if not filter then
        return ""
    end

    if picker.opts.live then
        return filter.search or ""
    end
    return filter.pattern or ""
end

---Build initial query options for the next picker from the current input text.
---Only the field the destination picker actually edits is seeded: live pickers (e.g. grep)
---edit `search`, while the others edit `pattern`. Seeding both leaves the non-edited field
---(e.g. `search` on the files picker) set, hidden and immutable — which traps the input.
---@param picker snacks.Picker
---@param target_live boolean whether the destination picker is live (edits `search`)
---@return snacks.picker.Config
local function picker_query_opts(picker, target_live)
    local input = picker_input_text(picker)
    if input == "" then
        return {}
    end
    if target_live then
        return { search = input }
    end
    return { pattern = input }
end

---Check whether the buffers picker would have at least one visible result.
---@param picker? snacks.Picker
---@return boolean
local function has_buffer_picker_results(picker)
    local current_buf = picker_current_buf(picker)
    for _, buf in ipairs(vim.api.nvim_list_bufs()) do
        local keep = vim.bo[buf].buflisted and buf ~= current_buf and vim.bo[buf].buftype ~= "nofile"
        if keep then
            return true
        end
    end
    return false
end

---Build recent picker options scoped to the active picker cwd.
---@param picker? snacks.Picker
---@return snacks.picker.Config
local function recent_picker_opts(picker)
    return {
        cwd = picker_cwd(picker),
        filter = { cwd = true },
    }
end

---Check whether the recent picker would have at least one visible result.
---@param picker? snacks.Picker
---@return boolean
local function has_recent_picker_results(picker)
    local opts = Snacks.picker.config.get(vim.tbl_extend("force", { source = "recent" }, recent_picker_opts(picker)))
    local current_buf = picker_current_buf(picker)
    local ok, found = pcall(vim.api.nvim_buf_call, current_buf, function()
        local filter = require("snacks.picker.core.filter").new({ opts = opts })
        local finder = require("snacks.picker.source.recent").files(opts, { filter = filter })
        local has_results = false
        finder(function()
            has_results = true
        end)
        return has_results
    end)
    return ok and found
end

---Open buffers, notifying when falling back to recent files or cwd files.
function M.open_buffers_or_recent_or_files()
    if has_buffer_picker_results() then
        Snacks.picker.buffers()
        return
    end

    if has_recent_picker_results() then
        vim.notify("󱎸 No buffers to show, opening recent files", vim.log.levels.INFO)
        Snacks.picker.recent(recent_picker_opts())
        return
    end

    vim.notify("󱙓 No buffers or recent files to show, opening files", vim.log.levels.INFO)
    LazyVim.pick("files", { root = false })()
end

local picker_switch_order = { "buffers", "recent", "files", "grep" }
local picker_switch_open = {
    files = function(opts)
        LazyVim.pick.open("files", opts)
    end,
    recent = function(opts)
        Snacks.picker.recent(opts)
    end,
    buffers = function(opts)
        Snacks.picker.buffers(opts)
    end,
    grep = function(opts)
        LazyVim.pick.open("live_grep", opts)
    end,
}
local picker_switch_has_results = {
    recent = has_recent_picker_results,
    buffers = has_buffer_picker_results,
}
-- Pickers whose input edits the live `search` field instead of the fuzzy `pattern` field.
local picker_switch_live = {
    grep = true,
}

---Build the option table used when switching to a picker source.
---@param picker snacks.Picker
---@param source string
---@return snacks.picker.Config
local function picker_switch_opts(picker, source)
    local query_opts = picker_query_opts(picker, picker_switch_live[source] == true)
    if source == "recent" then
        return vim.tbl_extend("force", recent_picker_opts(picker), query_opts)
    end
    if source == "files" or source == "grep" then
        return vim.tbl_extend("force", { root = false }, query_opts)
    end
    return query_opts
end

---Switch directly from the active picker to the requested picker source.
---@param picker snacks.Picker
---@param source string
local function switch_to_picker(picker, source)
    local open = picker_switch_open[source]
    local opts = picker_switch_opts(picker, source)
    switch_picker(picker, function()
        open(opts)
    end)
end

---Return whether rotation should stop at a picker source.
---@param picker snacks.Picker
---@param source string
---@return boolean
local function can_switch_to_picker(picker, source)
    local has_results = picker_switch_has_results[source]
    return not has_results or has_results(picker)
end

---Rotate through configured picker sources, skipping sources with no results.
---@param picker snacks.Picker
---@param step integer
local function rotate_picker(picker, step)
    local current = picker.opts.source
    local current_index = step > 0 and 0 or #picker_switch_order + 1
    for index, source in ipairs(picker_switch_order) do
        if source == current then
            current_index = index
            break
        end
    end

    for offset = 1, #picker_switch_order do
        local next_index = ((current_index - 1 + step * offset) % #picker_switch_order) + 1
        local source = picker_switch_order[next_index]
        if can_switch_to_picker(picker, source) then
            switch_to_picker(picker, source)
            return
        end
    end
end

---Switch from the active picker to the files picker.
---@param picker snacks.Picker
function M.switch_to_files(picker)
    switch_to_picker(picker, "files")
end

---Switch from the active picker to the buffers picker.
---@param picker snacks.Picker
function M.switch_to_buffers(picker)
    switch_to_picker(picker, "buffers")
end

---Switch from the active picker to the grep picker.
---@param picker snacks.Picker
function M.switch_to_grep(picker)
    switch_to_picker(picker, "grep")
end

---Switch from the active picker to the recent files picker.
---@param picker snacks.Picker
function M.switch_to_recent(picker)
    switch_to_picker(picker, "recent")
end

---Rotate from the active picker to the next available picker.
---@param picker snacks.Picker
function M.switch_to_next_picker(picker)
    rotate_picker(picker, 1)
end

---Rotate from the active picker to the previous available picker.
---@param picker snacks.Picker
function M.switch_to_prev_picker(picker)
    rotate_picker(picker, -1)
end

return M
