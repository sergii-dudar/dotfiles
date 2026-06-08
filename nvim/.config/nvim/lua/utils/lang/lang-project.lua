-- Agnostic project-language resolver.
--
-- Detects the active project's language from root markers so per-language plugin
-- configs (java/rust/...) can be loaded/gated in complete isolation: a Java
-- project never loads the Rust config and vice-versa. This keeps the same
-- `<leader>j…` muscle-memory usable per language without keymap collisions.
--
-- Used at lazy-spec build time (config/lazy.lua import `cond`) and inside the
-- per-language configs. Detection is by the nearest root marker to the cwd
-- (deepest root wins for polyglot/nested layouts) and cached for the session.
--
-- Extend for new languages by adding an entry to the `registry` table below
-- (e.g. `go = { "go.mod" }`).

local M = {}

---@type table<string, string[]>
local registry = {
    java = {
        "pom.xml", -- Maven
        "build.gradle", -- Gradle
        "build.gradle.kts", -- Gradle Kotlin DSL
        "settings.gradle", -- Gradle multi-project
        "settings.gradle.kts",
        "gradlew", -- Gradle wrapper
        "mvnw", -- Maven wrapper
        "build.xml", -- Ant
    },
    rust = {
        "Cargo.toml",
    },
}

local cached ---@type string|false|nil

--- Resolve the active project language from the cwd, or false if none matches.
--- Nearest (deepest) root marker wins so a nested project resolves correctly.
--- Honors the LIMITED feature gate. Cached per session.
---@return string|false
function current()
    if cached ~= nil then
        return cached
    end
    if _G.global and global.is_limited then
        cached = false
        return cached
    end

    local cwd = vim.fn.getcwd()
    local best_lang = false
    local best_len = -1
    for lang, markers in pairs(registry) do
        local root = vim.fs.root(cwd, markers)
        if root and #root > best_len then
            best_len = #root
            best_lang = lang
        end
    end

    cached = best_lang
    return cached
end

--- Is the active project the given language?
---@param lang string
---@return boolean
function M.is(lang)
    return current() == lang
end

return M
