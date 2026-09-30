--- Java diagnostic resolver registry and dispatcher.
---
--- Resolvers are registered by diagnostic-message Lua pattern. Each resolver
--- receives the matched diagnostic context and owns its UI/application flow.

local java_context = require("modules.java.diagnostics-resolver.java-context")

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

---@class JavaDiagnosticCandidate
---@field diagnostic table diagnostic at its current buffer position
---@field pattern string
---@field resolver function

--- Return a diagnostic at the position where Neovim currently renders it.
--- `vim.diagnostic.get()` reports the published position, which goes stale as soon
--- as lines are inserted above it. The location extmark follows the text, and jdtls
--- only republishes annotation-processor diagnostics after a rebuild.
---@param bufnr integer
---@param diagnostic table
---@return table|nil diagnostic nil when the diagnosed text no longer exists
local function at_current_position(bufnr, diagnostic)
    if type(diagnostic) ~= "table" then
        return nil
    end
    if not diagnostic._extmark_id or not diagnostic.namespace then
        return diagnostic
    end

    local ok, mark = pcall(function()
        local namespace = vim.diagnostic.get_namespace(diagnostic.namespace)
        return vim.api.nvim_buf_get_extmark_by_id(
            bufnr,
            namespace.user_data.location_ns,
            diagnostic._extmark_id,
            { details = true }
        )
    end)
    if not ok or type(mark) ~= "table" or type(mark[1]) ~= "number" then
        return diagnostic
    end

    local details = mark[3] or {}
    if details.invalid then
        return nil
    end

    local current = vim.deepcopy(diagnostic)
    current.lnum = mark[1]
    current.col = mark[2]
    current.end_lnum = details.end_row or mark[1]
    current.end_col = details.end_col or mark[2]
    return current
end

--- Collect the supported diagnostics accepted by a position filter.
--- Diagnostics published twice for the same position are offered once.
---@param diagnostics table[]
---@param accept fun(diagnostic: table): boolean
---@return JavaDiagnosticCandidate[]
local function supported_candidates(diagnostics, accept)
    local candidates = {}
    local seen = {}

    for _, diagnostic in ipairs(diagnostics) do
        if type(diagnostic.lnum) == "number" and accept(diagnostic) then
            local pattern, resolver = find_resolver(diagnostic)
            local key = table.concat({ diagnostic.lnum, tostring(diagnostic.col), diagnostic.message or "" }, "\0")
            if resolver and not seen[key] then
                seen[key] = true
                candidates[#candidates + 1] = {
                    diagnostic = diagnostic,
                    pattern = pattern,
                    resolver = resolver,
                }
            end
        end
    end

    return candidates
end

--- Run the resolver selected for a diagnostic.
---@param bufnr integer
---@param candidate JavaDiagnosticCandidate
local function dispatch(bufnr, candidate)
    candidate.resolver({
        bufnr = bufnr,
        diagnostic = candidate.diagnostic,
        pattern = candidate.pattern,
    })
end

--- Resolve a supported diagnostic on the current cursor line.
--- A cursor elsewhere inside a method, such as on one of its annotations, resolves
--- the diagnostics of that method. Several supported diagnostics open a picker.
---@return boolean resolved whether a resolver was dispatched or offered
function M.resolve_current()
    local bufnr = vim.api.nvim_get_current_buf()
    local cursor = vim.api.nvim_win_get_cursor(0)
    local cursor_lnum = cursor[1] - 1

    local diagnostics = {}
    for _, diagnostic in ipairs(vim.diagnostic.get(bufnr)) do
        diagnostics[#diagnostics + 1] = at_current_position(bufnr, diagnostic)
    end

    local candidates = supported_candidates(diagnostics, function(diagnostic)
        return cursor_lnum >= diagnostic.lnum and cursor_lnum <= (diagnostic.end_lnum or diagnostic.lnum)
    end)

    if #candidates == 0 then
        local start_row, end_row = java_context.method_rows_at(bufnr, cursor_lnum, cursor[2] or 0)
        if start_row and end_row then
            candidates = supported_candidates(diagnostics, function(diagnostic)
                return diagnostic.lnum >= start_row and diagnostic.lnum <= end_row
            end)
        end
    end

    if #candidates == 0 then
        vim.notify("[Java Diagnostics] No supported diagnostic on current line", vim.log.levels.INFO)
        return false
    end

    if #candidates == 1 then
        dispatch(bufnr, candidates[1])
        return true
    end

    vim.ui.select(candidates, {
        prompt = "Resolve Java diagnostic",
        format_item = function(candidate)
            return candidate.diagnostic.message
        end,
    }, function(candidate)
        if not candidate then
            return
        end
        -- The picker hands focus back asynchronously; resolvers edit the current window.
        vim.schedule(function()
            dispatch(bufnr, candidate)
        end)
    end)
    return true
end

-- Forged mappings of collection, stream, and map elements are described with an
-- element kind ("Collection element", "Map value", ...) instead of "property".
-- They must win over the generic unmapped-target resolvers, which would add the
-- annotations to the owning method instead of the forged element mapping.
M.register(
    'Unmapped target properties: ".*"%. Mapping from property ".*" to ".*"',
    require("modules.java.diagnostics-resolver.mapstruct-nested-properties-mapping-method").resolve
)
M.register(
    'Unmapped target properties: ".*"%. Mapping from %u%a+ %a+ ".*" to ".*"',
    require("modules.java.diagnostics-resolver.mapstruct-nested-properties-mapping-method").resolve
)
M.register(
    "Unmapped target properties: .*",
    require("modules.java.diagnostics-resolver.mapstruct-unmapped-target").resolve
)
M.register(
    'Unmapped target property: ".*"%. Mapping from property ".*" to ".*"',
    require("modules.java.diagnostics-resolver.mapstruct-nested-mapping-method").resolve
)
M.register(
    'Unmapped target property: ".*"%. Mapping from %u%a+ %a+ ".*" to ".*"',
    require("modules.java.diagnostics-resolver.mapstruct-nested-mapping-method").resolve
)
M.register(
    "Unmapped target property: .*",
    require("modules.java.diagnostics-resolver.mapstruct-unmapped-target").resolve
)
M.register(
    "Can't map property .*Consider to declare/implement a mapping method: .*",
    require("modules.java.diagnostics-resolver.mapstruct-mapping-method").resolve
)
M.register(
    "Can't map %u%a+ %a+ \".*Consider to declare/implement a mapping method: .*",
    require("modules.java.diagnostics-resolver.mapstruct-mapping-method").resolve
)
M.register(
    "Can't map parameter .*Consider to declare/implement a mapping method: .*",
    require("modules.java.diagnostics-resolver.mapstruct-parameter-mapping-method").resolve
)
M.register(
    '^The following constants from the property ".*" enum have no corresponding constant in the ".*" enum and must .-mapped via adding additional mappings: .*',
    require("modules.java.diagnostics-resolver.mapstruct-enum-mapping-method").resolve
)
-- Hints published by utils.java.jdtls-cleanup after a save ("Clean-up: → ..."): apply the hunk under the
-- cursor. Required lazily so this registry stays free of jdtls dependencies until a hint is resolved.
M.register("^Clean%-up: ", function(ctx)
    return require("utils.java.jdtls-cleanup").resolve_diagnostic(ctx)
end)

return M
