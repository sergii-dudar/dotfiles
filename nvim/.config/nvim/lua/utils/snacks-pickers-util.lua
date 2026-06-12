local M = {}

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
---@param picker snacks.Picker
---@return number
local function picker_current_buf(picker)
    local filter_buf = picker.input and picker.input.filter and picker.input.filter.current_buf
    if filter_buf and vim.api.nvim_buf_is_valid(filter_buf) then
        return filter_buf
    end

    local ok, main_buf = pcall(vim.api.nvim_win_get_buf, picker.main)
    if ok and vim.api.nvim_buf_is_valid(main_buf) then
        return main_buf
    end

    return vim.api.nvim_get_current_buf()
end

---Resolve the current working directory from the active picker context.
---@param picker snacks.Picker
---@return string
local function picker_cwd(picker)
    local filter_cwd = picker.input and picker.input.filter and picker.input.filter.cwd
    if filter_cwd then
        return filter_cwd
    end

    local ok, cwd = pcall(function()
        return picker:cwd()
    end)
    if ok and cwd then
        return cwd
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
---@param picker snacks.Picker
---@return snacks.picker.Config
local function picker_query_opts(picker)
    local input = picker_input_text(picker)
    if input == "" then
        return {}
    end
    return {
        pattern = input,
        search = input,
    }
end

---Check whether the buffers picker would have at least one visible result.
---@param picker snacks.Picker
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
---@param picker snacks.Picker
---@return snacks.picker.Config
local function recent_picker_opts(picker)
    return {
        cwd = picker_cwd(picker),
        filter = { cwd = true },
    }
end

---Check whether the recent picker would have at least one visible result.
---@param picker snacks.Picker
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

local picker_switch_order = { "files", "recent", "buffers", "grep" }
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

---Build the option table used when switching to a picker source.
---@param picker snacks.Picker
---@param source string
---@return snacks.picker.Config
local function picker_switch_opts(picker, source)
    local query_opts = picker_query_opts(picker)
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
