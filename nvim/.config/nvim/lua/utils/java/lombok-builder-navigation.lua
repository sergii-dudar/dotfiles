local M = {}

local ignored_builder_methods = {
    build = true,
    builder = true,
    toBuilder = true,
}

---@param line string
---@param idx integer
---@return string
local function char_at(line, idx)
    if idx < 1 or idx > #line then
        return ""
    end
    return line:sub(idx, idx)
end

---@param ch string
---@return boolean
local function is_identifier_char(ch)
    return ch:match("[%w_]") ~= nil
end

---@param line string
---@param idx integer
---@param step integer
---@return integer
local function skip_spaces(line, idx, step)
    while idx >= 1 and idx <= #line and char_at(line, idx):match("%s") do
        idx = idx + step
    end
    return idx
end

---@param bufnr integer
---@param lnum integer
---@return boolean
local function is_inside_builder_chain(bufnr, lnum)
    local start_lnum = math.max(lnum - 50, 1)
    local lines = vim.api.nvim_buf_get_lines(bufnr, start_lnum - 1, lnum, false)

    for idx = #lines, 1, -1 do
        local line = lines[idx]
        if line:match("%.builder%s*%(") or line:match("%.toBuilder%s*%(") then
            return true
        end

        if line:find(";") then
            return false
        end
    end

    return false
end

--- Return the chained Java method call under the cursor, if it looks builder-like.
---@return string|nil
local function get_chained_method_name_under_cursor()
    local lnum, col = unpack(vim.api.nvim_win_get_cursor(0))
    local line = vim.api.nvim_get_current_line()
    local cursor_idx = col + 1

    if not is_identifier_char(char_at(line, cursor_idx)) then
        return nil
    end

    local start_idx = cursor_idx
    while start_idx > 1 and is_identifier_char(char_at(line, start_idx - 1)) do
        start_idx = start_idx - 1
    end

    local end_idx = cursor_idx
    while end_idx < #line and is_identifier_char(char_at(line, end_idx + 1)) do
        end_idx = end_idx + 1
    end

    local name = line:sub(start_idx, end_idx)
    if name == "" or ignored_builder_methods[name] then
        return nil
    end

    local prev_idx = skip_spaces(line, start_idx - 1, -1)
    if char_at(line, prev_idx) ~= "." then
        return nil
    end

    local next_idx = skip_spaces(line, end_idx + 1, 1)
    if char_at(line, next_idx) ~= "(" then
        return nil
    end

    if not is_inside_builder_chain(vim.api.nvim_get_current_buf(), lnum) then
        return nil
    end

    return name
end

---@param line string
---@return integer
local function brace_delta(line)
    local opens = select(2, line:gsub("{", ""))
    local closes = select(2, line:gsub("}", ""))
    return opens - closes
end

---@param line string
---@param field_name string
---@return integer|nil
local function field_column(line, field_name)
    if not line:find("[;=]") then
        return nil
    end

    local start_idx, end_idx = line:find("%f[%w_]" .. field_name .. "%f[^%w_]")
    if not start_idx or not end_idx then
        return nil
    end

    local next_idx = skip_spaces(line, end_idx + 1, 1)
    if char_at(line, next_idx) == "(" then
        return nil
    end

    return start_idx - 1
end

---@param bufnr integer
---@param field_name string
---@param annotation_lnum integer
---@return integer|nil, integer|nil
local function find_field_in_builder_class(bufnr, field_name, annotation_lnum)
    local lines = vim.api.nvim_buf_get_lines(bufnr, 0, -1, false)
    local brace_depth = 0
    for idx = 1, annotation_lnum - 1 do
        brace_depth = brace_depth + brace_delta(lines[idx] or "")
    end

    local found_class = false
    local class_depth = brace_depth

    for idx = annotation_lnum, #lines do
        local line = lines[idx]
        local before_depth = brace_depth

        if not found_class and line:match("%f[%w_]class%s+") then
            found_class = true
            class_depth = before_depth
        elseif found_class and before_depth == class_depth + 1 then
            local col = field_column(line, field_name)
            if col then
                return idx, col
            end
        end

        brace_depth = brace_depth + brace_delta(line)
        if found_class and idx > annotation_lnum and brace_depth <= class_depth then
            break
        end
    end

    return nil, nil
end

---@param win integer
---@param bufnr integer
---@param field_name string
---@return boolean
local function jump_from_builder_annotation_to_field(win, bufnr, field_name)
    if not vim.api.nvim_win_is_valid(win) or not vim.api.nvim_buf_is_valid(bufnr) then
        return false
    end

    local cursor = vim.api.nvim_win_get_cursor(win)
    local lnum = cursor[1]
    local line = vim.api.nvim_buf_get_lines(bufnr, lnum - 1, lnum, false)[1] or ""
    if not line:match("@Builder") then
        return false
    end

    local field_lnum, col = find_field_in_builder_class(bufnr, field_name, lnum)
    if not field_lnum then
        return false
    end

    vim.api.nvim_win_set_cursor(win, { field_lnum, col or 0 })
    vim.api.nvim_win_call(win, function()
        vim.cmd("normal! zv")
        vim.cmd("normal! zz")
    end)
    return true
end

---@param options vim.lsp.LocationOpts.OnList
---@param field_name string
---@param win integer
---@param tagname string
---@param from table
local function apply_definition_list(options, field_name, win, tagname, from)
    local items = options.items or {}
    if vim.tbl_isempty(items) then
        vim.notify("No locations found", vim.log.levels.INFO)
        return
    end

    if #items ~= 1 then
        vim.fn.setqflist({}, " ", options)
        vim.cmd("botright copen")
        return
    end

    local item = items[1]
    local bufnr = item.bufnr or vim.fn.bufadd(item.filename)

    vim.cmd("normal! m'")
    vim.fn.settagstack(vim.fn.win_getid(win), { items = { { tagname = tagname, from = from } } }, "t")

    vim.bo[bufnr].buflisted = true
    vim.api.nvim_win_set_buf(win, bufnr)
    vim.api.nvim_win_set_cursor(win, { item.lnum, item.col - 1 })
    vim.api.nvim_win_call(win, function()
        vim.cmd("normal! zv")
    end)

    jump_from_builder_annotation_to_field(win, bufnr, field_name)
end

--- Run standard LSP definition, then redirect Lombok builder annotation hits to the field.
---@param ctx lang.LspNavigationContext
---@return boolean
function M.goto_definition(ctx)
    if ctx.filetype ~= "java" then
        return false
    end

    local field_name = get_chained_method_name_under_cursor()
    if not field_name then
        return false
    end

    local win = vim.api.nvim_get_current_win()
    local from = vim.fn.getpos(".")
    from[1] = ctx.bufnr
    local tagname = field_name

    vim.lsp.buf.definition({
        on_list = function(options)
            apply_definition_list(options, field_name, win, tagname, from)
        end,
    })

    return true
end

return M
