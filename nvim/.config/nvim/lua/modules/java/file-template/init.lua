-- Java adapter for the generic file-template engine.
--
-- Adds Java specific match context (package, maven/gradle source set) and points
-- the engine at the Java rule table. Registers itself on require, mirroring the
-- test-report adapter pattern.
--
-- Templates are resolved against the `java-template` LuaSnip filetype first
-- (file-only templates that must not show up in normal completion), then the
-- regular `java` snippets, so existing snippets like `util`, `usecase`,
-- `component`, `controller`, `mapper`, `immutable` are reused as-is.

require("modules.common.file-template.types")

local M = {}

local registry = require("modules.common.file-template.registry")

M.lang = "java"
M.filetypes = { "java" }
M.snippet_filetypes = { "java-template", "java" }

--- Maven/Gradle source layout: `<module>/src/<source-set>/java/<package>/<File>.java`.
local SOURCE_ROOT_PATTERN = "/src/([^/]+)/java/"

--- Resolve the dotted package and source set of a Java file from its path.
--- Uses the last `src/<set>/java/` occurrence so nested module paths that
--- themselves contain `src` still resolve correctly.
---@param path string
---@return string|nil package, string|nil source_set
function M.resolve_package(path)
    local package_start, source_set
    local search_from = 1

    while true do
        local match_start, match_end, set = path:find(SOURCE_ROOT_PATTERN, search_from)
        if not match_start then
            break
        end
        package_start, source_set = match_end + 1, set
        search_from = match_start + 1
    end

    if not package_start then
        return nil, nil
    end

    local relative = path:sub(package_start)
    local package_dir = relative:match("^(.*)/[^/]+$")
    if not package_dir or package_dir == "" then
        -- File sits directly in the source root (default package).
        return "", source_set
    end

    return (package_dir:gsub("/", ".")), source_set
end

---@param ctx file_template.Context
---@return table
function M.context(ctx)
    local package, source_set = M.resolve_package(ctx.path)
    return {
        package = package,
        source_set = source_set,
        is_test = source_set ~= nil and source_set ~= "main",
    }
end

--- Only act on files inside a real Java source root, which keeps templates out
--- of stray `.java` files opened from decompiled sources, scratch dirs, etc.
---@param ctx file_template.Context
---@return boolean
function M.enabled(ctx)
    return ctx.package ~= nil
end

---@return file_template.Rule[]
function M.rules()
    return require("modules.java.file-template.rules")
end

registry.register(M)

return M
