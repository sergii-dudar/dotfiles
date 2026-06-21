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
-- - current - resolve/cache the active project language
-- - reset - clear cached detection
-- - is - compare the active project language with a requested language
--
-- Detection tiers (first match wins):
--   1. Root markers nearest the cwd (deepest root wins for nested/polyglot).
--   2. Fallback for marker-less folders (e.g. a plain dir of loose *.java files):
--      a. extension of the opened file(s) (nvim argv / current buffer)
--      b. a bounded, heavy-dir-skipping scan of the cwd for source files
--   Tier 2 only runs when tier 1 matched nothing, so it never overrides a real
--   project root and a plain `cd ~` won't be misdetected (bounded depth).
--
-- Extend for new languages by adding a primary entry to `utils.lang.registry`.

local M = {}
local lang_registry = require("utils.lang.registry")

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

--- Extract a filename extension without the leading dot.
---@param name string
---@return string|nil
local function ext_of(name)
    return name:match("%.([%w_]+)$")
end

--- Resolve the primary language registered for a source extension.
---@param ext string|nil
---@return string|nil
local function lang_for_ext(ext)
    if not ext then
        return nil
    end
    for _, entry in ipairs(lang_registry.primary()) do
        if entry.project.exts[ext] then
            return entry.name
        end
    end
    return nil
end

--- Detect by nearest project root marker (deepest path wins).
--
-- For each primary language `vim.fs.root` walks upward from `cwd` and returns the
-- closest ancestor directory that holds one of that language's markers. Several
-- languages may match at different depths (e.g. a Java sub-project nested inside a
-- Rust workspace), so we must disambiguate instead of trusting registry order.
--
-- `best_len` tracks the longest matched root path seen so far. Since every match is
-- an ancestor of the same `cwd`, a deeper directory always yields a longer path
-- string, so the longest path is the nearest (most specific) root — that language
-- wins. This keeps detection correct for nested projects regardless of iteration
-- order.
---@param cwd string
---@return string|nil
local function detect_by_markers(cwd)
    local best_lang, best_len = nil, -1
    for _, entry in ipairs(lang_registry.primary()) do
        local root = vim.fs.root(cwd, entry.project.markers)
        if root and #root > best_len then
            best_len = #root
            best_lang = entry.name
        end
    end
    return best_lang
end

--- Detect by extension of the file(s) the editor was opened with.
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

--- Detect by bounded, heavy-dir-skipping scan of cwd for a known source file.
---@param cwd string
---@return string|nil
local function detect_by_sources(cwd)
    local result
    --- Walk directories until a registered source extension is found.
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
    -- LIMITED mode (`LIMITED` env var, exposed via `_G.global.is_limited`) is a
    -- session-wide opt-out for heavy, per-language IDE tooling — used when
    -- launching nvim purely as an editor (quick edits, diff viewer, git
    -- commit). Short-circuiting to `false` here prevents any primary editor
    -- config from being gated on, so projects load the generic baseline only.
    if _G.global and global.is_limited then
        cached = false
        return cached
    end

    local cwd = vim.fn.getcwd()
    cached = detect_by_markers(cwd) or detect_by_open_files() or detect_by_sources(cwd) or false
    return cached
end

--- Clear the cached detection result so the next `current()` re-resolves.
--- Not called automatically: editor-config gating happens at lazy import time, so a
--- mid-session `:cd` into another project intentionally does NOT re-gate. This is a
--- manual / REPL / test escape hatch (e.g. after `:cd`, run
--- `:lua require("utils.lang.lang-project").reset()`).
function M.reset()
    cached = nil
end

--- Is the active project the given language?
---@param lang string
---@return boolean
function M.is(lang)
    return M.current() == lang
end

return M
