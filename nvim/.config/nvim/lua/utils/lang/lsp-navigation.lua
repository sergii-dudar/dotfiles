-- LSP navigation utilities: route definition/declaration requests through
-- optional language-specific handlers before falling back to the default LSP
-- implementation.
--
-- - definition - run custom `gd` navigation handlers with LSP fallback
-- - declaration - run custom `gD` navigation handlers with LSP fallback

local lang_registry = require("utils.lang.registry")

local M = {}
local lsp_modules_by_lang = {}

---@alias lang.LspNavigationMethod "definition"|"declaration"

---@class lang.LspNavigationContext
---@field method lang.LspNavigationMethod LSP navigation method being invoked.
---@field bufnr integer Current buffer number.
---@field filetype string Current buffer filetype.
---@field row integer Current cursor row, 0-indexed.
---@field col integer Current cursor column, 0-indexed.
---@field fallback fun() Continue with the standard LSP navigation request.

---@class lang.LspNavigationHandler
---@field name? string Human-readable handler name used in error notifications.
---@field navigate fun(ctx: lang.LspNavigationContext): boolean|nil Return true when the handler owns the request.

local fallback_by_method = {
    definition = function()
        vim.lsp.buf.definition()
    end,
    declaration = function()
        vim.lsp.buf.declaration()
    end,
}

--- Resolve the standard LSP fallback for a navigation method.
---@param method lang.LspNavigationMethod
---@return fun()|nil
local function fallback_for(method)
    return fallback_by_method[method]
end

--- Load the language-specific LSP metadata module for a filetype.
---@param filetype string
---@return table|nil
local function get_lang_lsp_module(filetype)
    local entry = lang_registry.for_filetype(filetype)
    if not entry then
        return nil
    end

    if lsp_modules_by_lang[entry.name] ~= nil then
        return lsp_modules_by_lang[entry.name] or nil
    end

    local module_name = ("utils.lang.%s.lsp-%s"):format(entry.name, entry.name)
    local ok, mod = pcall(require, module_name)
    if ok then
        lsp_modules_by_lang[entry.name] = mod
        return mod
    end

    lsp_modules_by_lang[entry.name] = false
    return nil
end

--- Get navigation handlers declared by the active language module.
---@param ctx lang.LspNavigationContext
---@return lang.LspNavigationHandler[]
local function get_handlers(ctx)
    local mod = get_lang_lsp_module(ctx.filetype)
    local navigation = mod and mod.navigation
    if not navigation then
        return {}
    end

    local handlers = navigation[ctx.method]
    if type(handlers) == "function" then
        handlers = handlers(ctx)
    end

    return handlers or {}
end

--- Run one navigation handler and isolate failures from normal LSP fallback.
---@param handler lang.LspNavigationHandler|fun(ctx: lang.LspNavigationContext): boolean|nil
---@param ctx lang.LspNavigationContext
---@return boolean
local function try_handler(handler, ctx)
    local navigate
    if type(handler) == "function" then
        navigate = handler
    elseif type(handler) == "table" then
        navigate = handler.navigate
    end

    if not navigate then
        return false
    end

    local ok, handled = pcall(navigate, ctx)
    if not ok then
        local name = type(handler) == "table" and handler.name or ctx.method
        vim.notify(("LSP navigation handler failed [%s]: %s"):format(name or ctx.method, handled), vim.log.levels.WARN)
        return false
    end

    return handled == true
end

--- Run custom handlers for a navigation method before the standard LSP request.
---@param method lang.LspNavigationMethod
local function run(method)
    local fallback = fallback_for(method)
    if not fallback then
        vim.notify("Unsupported LSP navigation method: " .. method, vim.log.levels.WARN)
        return
    end

    local bufnr = vim.api.nvim_get_current_buf()
    local cursor = vim.api.nvim_win_get_cursor(0)
    local did_fallback = false

    --- Continue with standard LSP navigation once.
    local function continue_lsp()
        if did_fallback then
            return
        end
        did_fallback = true
        fallback()
    end

    ---@type lang.LspNavigationContext
    local ctx = {
        method = method,
        bufnr = bufnr,
        filetype = vim.bo[bufnr].filetype,
        row = cursor[1] - 1,
        col = cursor[2],
        fallback = continue_lsp,
    }

    for _, handler in ipairs(get_handlers(ctx)) do
        if try_handler(handler, ctx) then
            return
        end
    end

    continue_lsp()
end

--- Navigate to a symbol definition through registered language-specific handlers.
--- Used by `gd` mappings; falls back to `vim.lsp.buf.definition()` exactly once
--- when no handler claims the request.
function M.definition()
    run("definition")
end

--- Navigate to a symbol declaration through registered language-specific handlers.
--- Used by `gD` mappings; falls back to `vim.lsp.buf.declaration()` exactly once
--- when no handler claims the request.
function M.declaration()
    run("declaration")
end

return M
