-- LuaSnip bridge for the file-template engine.
--
-- Templates are ordinary LuaSnip snippets looked up by trigger, so a template is
-- never duplicated: the same snippet can be expanded manually while typing and
-- automatically into a new file. Expansion keeps insert/choice nodes live, so
-- the user lands inside the template with jumps ready.

local M = {}

--- Force-load LuaSnip so lazy.nvim runs its `config` (which registers our
--- custom snippets) before we query the snippet collection.
---@return table|nil luasnip
local function ensure_luasnip()
    local ok_lazy, lazy = pcall(require, "lazy")
    if ok_lazy then
        pcall(lazy.load, { plugins = { "LuaSnip" } })
    end

    local ok, luasnip = pcall(require, "luasnip")
    if not ok then
        return nil
    end
    return luasnip
end

--- Find a snippet by trigger, searching the given LuaSnip filetypes in order.
---@param filetypes string[]
---@param trigger string
---@return table|nil snippet, table|nil luasnip
function M.find(filetypes, trigger)
    local luasnip = ensure_luasnip()
    if not luasnip then
        return nil, nil
    end

    for _, filetype in ipairs(filetypes) do
        for _, snippet in ipairs(luasnip.get_snippets(filetype) or {}) do
            if snippet.trigger == trigger then
                return snippet, luasnip
            end
        end
    end
    return nil, luasnip
end

---@param filetypes string[]
---@param trigger string
---@return boolean
function M.exists(filetypes, trigger)
    return M.find(filetypes, trigger) ~= nil
end

--- Replace the current buffer content with the expanded template.
--- Must run with the target buffer focused in the current window.
---@param filetypes string[]
---@param trigger string
---@param opts? { choice?: integer }
---@return boolean ok, string|nil err
function M.expand(filetypes, trigger, opts)
    opts = opts or {}

    local snippet, luasnip = M.find(filetypes, trigger)
    if not luasnip then
        return false, "LuaSnip is not available"
    end
    if not snippet then
        return false, ("template snippet %q not found in %s"):format(trigger, table.concat(filetypes, ", "))
    end

    local bufnr = vim.api.nvim_get_current_buf()
    vim.api.nvim_buf_set_lines(bufnr, 0, -1, false, { "" })
    vim.api.nvim_win_set_cursor(0, { 1, 0 })

    -- `snip_expand` copies the snippet internally, so the registered one is not consumed.
    -- `indent` is intentionally left at its default: LuaSnip only computes
    -- `parent.indentstr` when indenting, and function nodes require it. Expansion
    -- always happens at column 0 of a blank buffer, so the indent prefix is empty.
    local ok, err = pcall(luasnip.snip_expand, snippet, { pos = { 0, 0 } })
    if not ok then
        return false, tostring(err)
    end

    if opts.choice and luasnip.choice_active() then
        pcall(luasnip.set_choice, opts.choice)
    end

    return true, nil
end

return M
