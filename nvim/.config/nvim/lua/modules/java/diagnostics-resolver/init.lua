--- Java diagnostic resolver registry and dispatcher.
---
--- Resolvers are registered by diagnostic-message Lua pattern. Each resolver
--- receives the matched diagnostic context and owns its UI/application flow.

local M = {}

local resolvers = {}
local patterns = {}

--- Register a resolver for a diagnostic message pattern.
---@param pattern string Lua pattern matched against `diagnostic.message`
---@param resolver fun(ctx: table): boolean|nil
function M.register(pattern, resolver)
    if not resolvers[pattern] then
        patterns[#patterns + 1] = pattern
    end
    resolvers[pattern] = resolver
end

--- Find the first resolver matching a diagnostic message.
---@param diagnostic table
---@return string|nil pattern
---@return function|nil resolver
local function find_resolver(diagnostic)
    local message = diagnostic and diagnostic.message or ""
    for _, pattern in ipairs(patterns) do
        if message:match(pattern) then
            return pattern, resolvers[pattern]
        end
    end
    return nil, nil
end

--- Resolve the first supported diagnostic on the current cursor line.
---@return boolean resolved whether a resolver was dispatched
function M.resolve_current()
    local bufnr = vim.api.nvim_get_current_buf()
    local cursor_lnum = vim.api.nvim_win_get_cursor(0)[1] - 1
    local diagnostics = vim.diagnostic.get(bufnr, { lnum = cursor_lnum })

    for _, diagnostic in ipairs(diagnostics) do
        local pattern, resolver = find_resolver(diagnostic)
        if resolver then
            resolver({
                bufnr = bufnr,
                diagnostic = diagnostic,
                pattern = pattern,
            })
            return true
        end
    end

    vim.notify("[Java Diagnostics] No supported diagnostic on current line", vim.log.levels.INFO)
    return false
end

M.register(
    "Unmapped target properties: .*",
    require("modules.java.diagnostics-resolver.mapstruct-unmapped-target").resolve
)

return M
