-- Agnostic project-language resolver.
--
-- Detects the active project's language so per-language plugin configs
-- (java/rust/...) can be loaded/gated in complete isolation: a Java project never
-- loads the Rust config and vice-versa. This keeps the same `<leader>j…`
-- muscle-memory usable per language without keymap collisions.
--
-- Used at lazy-spec build time (config/lazy.lua import `cond`) and inside the
-- per-language configs. Result is cached for the session.
--
-- Detection tiers (first match wins):
--   1. Root markers nearest the cwd (deepest root wins for nested/polyglot).
--   2. Fallback for marker-less folders (e.g. a plain dir of loose *.java files):
--      a. extension of the opened file(s) (nvim argv / current buffer)
--      b. a bounded, heavy-dir-skipping scan of the cwd for source files
--   Tier 2 only runs when tier 1 matched nothing, so it never overrides a real
--   project root and a plain `cd ~` won't be misdetected (bounded depth).
--
-- Extend for new languages by adding an entry to the `registry` table below.

local M = {}

---@class lang_project.Def
---@field markers string[]            Root-marker filenames (for vim.fs.root).
---@field exts table<string, boolean> Source-file extensions (without dot).

---@type table<string, lang_project.Def>
local registry = {
    java = {
        markers = {
            "pom.xml", -- Maven
            "build.gradle", -- Gradle
            "build.gradle.kts", -- Gradle Kotlin DSL
            "settings.gradle", -- Gradle multi-project
            "settings.gradle.kts",
            "gradlew", -- Gradle wrapper
            "mvnw", -- Maven wrapper
            "build.xml", -- Ant
        },
        exts = { java = true },
    },
    rust = {
        markers = { "Cargo.toml" },
        exts = { rs = true },
    },
}

-- Tier-2 source scan bounds (keep the marker-less fallback cheap and safe).
local SCAN_MAX_DEPTH = 4
local SCAN_SKIP = {
    [".git"] = true,
    [".idea"] = true,
    node_modules = true,
    target = true,
    build = true,
    bin = true,
    obj = true,
    out = true,
}

local cached ---@type string|false|nil

---@param name string
---@return string|nil
local function ext_of(name)
    return name:match("%.([%w_]+)$")
end

---@param ext string|nil
---@return string|nil
local function lang_for_ext(ext)
    if not ext then
        return nil
    end
    for lang, def in pairs(registry) do
        if def.exts[ext] then
            return lang
        end
    end
    return nil
end

-- Tier 1: nearest root marker (deepest path wins).
---@param cwd string
---@return string|nil
local function detect_by_markers(cwd)
    local best_lang, best_len = nil, -1
    for lang, def in pairs(registry) do
        local root = vim.fs.root(cwd, def.markers)
        if root and #root > best_len then
            best_len = #root
            best_lang = lang
        end
    end
    return best_lang
end

-- Tier 2a: extension of the file(s) the editor was opened with.
---@return string|nil
local function detect_by_open_files()
    for _, f in ipairs(vim.fn.argv()) do
        local lang = lang_for_ext(ext_of(f))
        if lang then
            return lang
        end
    end
    local bufname = vim.api.nvim_buf_get_name(0)
    if bufname ~= "" then
        return lang_for_ext(ext_of(bufname))
    end
    return nil
end

-- Tier 2b: bounded, heavy-dir-skipping scan of cwd for a known source file.
---@param cwd string
---@return string|nil
local function detect_by_sources(cwd)
    local result
    local function walk(dir, depth)
        if result then
            return
        end
        for name, type in vim.fs.dir(dir) do
            if result then
                return
            end
            if type == "file" then
                local lang = lang_for_ext(ext_of(name))
                if lang then
                    result = lang
                    return
                end
            elseif type == "directory" and depth < SCAN_MAX_DEPTH and not SCAN_SKIP[name] then
                walk(dir .. "/" .. name, depth + 1)
            end
        end
    end
    walk(cwd, 0)
    return result
end

--- Resolve the active project language, or false if none matches.
--- Honors the LIMITED feature gate. Cached per session.
---@return string|false
function M.current()
    if cached ~= nil then
        return cached
    end
    if _G.global and global.is_limited then
        cached = false
        return cached
    end

    local cwd = vim.fn.getcwd()
    cached = detect_by_markers(cwd) or detect_by_open_files() or detect_by_sources(cwd) or false
    return cached
end

--- Is the active project the given language?
---@param lang string
---@return boolean
function M.is(lang)
    return M.current() == lang
end

return M
