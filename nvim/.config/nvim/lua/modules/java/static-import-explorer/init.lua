--- Static import resolver for Java members that JDTLS does not auto-import.
---
--- Provides two user-facing workflows:
--- - `M.quick_import()`: fast ripgrep lookup for the word under cursor, with
---   automatic insertion when there is one import candidate.
--- - `M.find()`: interactive Snacks picker for browsing static members and
---   widening the search scope.
---
--- Search scope:
--- - Quick import loads dependency sources, then searches current module Java
---   sources plus `settings.preferred_deps_main` / `settings.preferred_deps_test`.
---   This keeps the fast path small and predictable.
--- - Full picker starts with module sources plus preferred dependencies when
---   dependency sources are already loaded. Inside the picker, `<C-d>` toggles
---   filtered dependency dirs and `<C-a>` toggles all dependency dirs.
--- - Preferred dependency entries use the same `groupId` / `groupId:artifactId`
---   coordinate style for Maven and Gradle caches. Entries also support `!`
---   exclusions and `#sub/path;other/path` subpath restrictions.
---
--- Public API:
--- - `M.quick_import()`: Resolve and insert a static import for the word under cursor.
--- - `M.find()`: Open the interactive static-import picker for the word under cursor.

local util = require("modules.java.static-import-explorer.util")
local picker = require("modules.java.static-import-explorer.picker")
local dep_search = require("modules.java.dependencies-search")
local java_util = require("utils.java.java-common")

local M = {}

local settings = {
    -- "wildcard": import static pkg.Class.*;
    -- "explicit": import static pkg.Class.memberName;
    import_mode = "explicit",
    -- auto-import without showing multiselect when only one result found
    auto_apply_single = true,
    -- fallback to find() when quick has no results or user cancels select
    fallback_to_find = true,
    -- include generated Java sources from target/generated-sources and build/generated/sources
    include_generated_sources = false,
    -- Preferred dependencies included in default search alongside module src/.
    -- Format: "groupId" or "groupId:artifactId" for both Maven and Gradle caches.
    -- Prefix with "!" to exclude a matched dependency, or append "#sub/path;other/path"
    -- to restrict a dependency to selected packages/directories.
    preferred_deps_main = {
        -- "org.apache.commons",
        -- "com.fasterxml.jackson.core:jackson-databind",
        -- "org.apache.commons.collections4",
        -- "org.apache.commons.lang3",
        -- "org.apache.commons.commons-text",
        "org.apache.commons",
        "!org.apache.commons.commons-compress",
        "com.google.guava.guava",
    },
    -- test scope: these + preferred_deps_main (merged below)
    preferred_deps_test = {
        -- "org.assertj",
        "org.assertj:assertj-core#org/assertj/core/util;org/assertj/core/api",
        "org.mockito.mockito-core",
    },
}

-- Test scope always includes main preferred deps
vim.list_extend(settings.preferred_deps_test, settings.preferred_deps_main)

--- Create isolated picker/search state for one user invocation.
--- The state keeps the source buffer, current word, picker toggles, active glob,
--- and a per-invocation FQCN cache so quick-import and picker paths do not share
--- stale buffer-specific data.
---@param opts? table
---@return table
local function new_state(opts)
    -- Capture the identifier and its buffer range together so prefix completion
    -- (e.g. cursor on SIGNE -> SIGNED_NUM) can rewrite exactly the typed text.
    local word_range, cword = util.cword_range()
    local state = {
        source_bufnr = vim.api.nvim_get_current_buf(),
        source_file = vim.api.nvim_buf_get_name(0),
        include_deps = false,
        include_all_deps = false,
        starts_with = false,
        current_word = cword or "",
        word_range = word_range,
        default_glob = "*.java",
        fqcn_cache = {},
    }
    if opts then
        state = vim.tbl_extend("force", state, opts)
    end
    return state
end

--- Insert selected static-import items into the invocation source buffer.
---@param state table invocation state containing `source_bufnr`
---@param items { name: string }[] parsed import candidates
local function apply_items(state, items)
    for _, item in ipairs(items) do
        -- Complete the typed prefix to the full member before inserting the import:
        -- the import line goes above the usage and would shift the captured row.
        util.complete_word_in_buffer(state.source_bufnr, state.word_range, state.current_word, item.member)
        util.add_import_to_buffer(item.name, state.source_bufnr)
    end
end

--- Format a quick-import choice for `vim.ui.select`.
---@param item { fqcn: string, member?: string } parsed import candidate
---@return string label select-menu label in `Class.member (package)` form
local function format_select_item(item)
    local class = item.fqcn:match("([^%.]+)$") or item.fqcn
    local pkg = item.fqcn:match("^(.+)%.") or ""
    local member_str = item.member or "*"
    return class .. "." .. member_str .. "  (" .. pkg .. ")"
end

--- Resolve dependency scope from the invocation source buffer.
---@param state table invocation state containing `source_bufnr`
---@return "main"|"test" scope preferred/dependency scope to use for this buffer
local function get_scope(state)
    return java_util.is_test_file(state.source_bufnr) and "test" or "main"
end

--- Build quick-import search directories from module sources and preferred deps.
--- This intentionally excludes full dependency source dirs for speed; the picker
--- path can widen scope with `<C-d>` / `<C-a>` when the focused search misses.
---@param state table invocation state containing `source_bufnr`
---@return string[] dirs deduplicated source directories for the fast ripgrep pass
local function get_quick_search_dirs(state)
    local dirs = util.get_module_src_dirs(state.source_bufnr, settings.include_generated_sources)
    vim.list_extend(dirs, util.get_preferred_dep_dirs(get_scope(state), settings))
    return util.dedup_dirs(dirs)
end

--- Open the full picker after a quick-import miss or cancelled selection.
--- The fallback starts in all-dependencies mode so missed preferred-dep results
--- are discoverable without requiring another keypress.
---@param state table invocation state reused by the picker
---@param word string searched word under cursor
local function fallback_to_find(state, word)
    if not settings.fallback_to_find then
        return
    end
    state.current_word = word
    state.include_all_deps = true
    state.include_deps = false
    if not dep_search.is_loaded() then
        dep_search.load_sources({
            bufnr = state.source_bufnr,
            on_done = function()
                picker.open(settings, state)
            end,
        })
    else
        picker.open(settings, state)
    end
end

--- Handle ripgrep completion for quick-import.
--- Parses ripgrep output, auto-applies a single candidate when configured, shows
--- a select menu for multiple candidates, or falls back to the full picker.
---@param state table invocation state
---@param result { code: integer, stdout?: string } `vim.system` result
---@param word string searched word under cursor
local function on_rg_result(state, result, word)
    if result.code ~= 0 or not result.stdout or result.stdout == "" then
        vim.notify("[Static Import] No matches found", vim.log.levels.INFO)
        fallback_to_find(state, word)
        return
    end

    local items = util.parse_rg_results(result.stdout, settings.import_mode, word, state.fqcn_cache, state.source_file)
    if #items == 0 then
        vim.notify("[Static Import] No valid matches found", vim.log.levels.INFO)
        fallback_to_find(state, word)
        return
    end

    if #items == 1 and settings.auto_apply_single then
        apply_items(state, items)
        return
    end

    vim.ui.select(items, {
        prompt = "Static Imports (" .. #items .. ")",
        format_item = format_select_item,
    }, function(choice)
        if choice then
            apply_items(state, { choice })
        else
            fallback_to_find(state, word)
        end
    end)
end

--- Run the quick-import ripgrep search for the invocation state.
---@param state table invocation state
---@param word string searched word under cursor
---@param pattern string ripgrep pattern from `util.build_search`
local function run_search(state, word, pattern)
    local search_dirs = get_quick_search_dirs(state)
    if #search_dirs == 0 then
        vim.notify("[Static Import] No search directories found", vim.log.levels.WARN)
        return
    end

    -- dd(search_dirs)
    local rg_cmd = { "rg", "-n", "--no-heading", "-e", pattern, "--glob", "*.java" }
    vim.list_extend(rg_cmd, search_dirs)

    vim.system(
        rg_cmd,
        { text = true },
        vim.schedule_wrap(function(result)
            on_rg_result(state, result, word)
        end)
    )
end

--- Open the interactive Snacks picker for browsing/selecting static imports.
--- Seeds the prompt with the word under cursor and searches the module source
--- dirs plus preferred dependency dirs (scope-aware: main vs test). The picker
--- exposes toggles for including filtered/all dependency sources and for
--- starts-with vs substring matching. Selected entries are inserted as
--- `import static` lines via `util.add_import_to_buffer`.
---
--- When to use: exploratory lookup — you don't know the exact member name, want
--- to fuzzy-search across many candidates, or need to widen the search to all
--- dependencies via the picker's toggles.
function M.find()
    local state = new_state()

    -- picker.open computes the search dirs and warns/returns if there are none.
    picker.open(settings, state)
end

--- One-shot static import resolver for the identifier under the cursor.
--- Builds a starts-with regex (field/enum-constant for ALL_CAPS, method for
--- camelCase) and runs `rg` across module src + preferred dependency dirs
--- (loading dep sources first if needed). Behaviour by match count:
---   • 0 matches  -> falls back to the full picker via `fallback_to_find`
---     (which expands the search to all dependencies)
---   • 1 match    -> auto-applied when `settings.auto_apply_single` is set
---   • N matches  -> shown via `vim.ui.select`; cancelling falls back to find
--- Intended as the fast keymap path; `M.find` is the explicit/explorer entry.
---
--- When to use: cursor sits on a known unresolved static reference (e.g.
--- `assertThat`, `MAX_VALUE`, `mock`) and you want a single keystroke to import
--- it without leaving the buffer.
function M.quick_import()
    local state = new_state()
    local word = state.current_word

    if word == "" then
        vim.notify("[Static Import] No word under cursor", vim.log.levels.WARN)
        return
    end

    local pattern = util.build_search(word, true)
    if not pattern then
        return
    end

    if not dep_search.is_loaded() then
        dep_search.load_sources({
            bufnr = state.source_bufnr,
            on_done = function()
                run_search(state, word, pattern)
            end,
        })
    else
        run_search(state, word, pattern)
    end
end

return M
