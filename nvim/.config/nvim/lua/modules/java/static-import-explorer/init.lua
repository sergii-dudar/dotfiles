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
    include_generated_sources = true,
    -- Preferred dependencies included in default search alongside src/.
    -- Format: "groupId" or "groupId:artifactId" (same as dependencies-search include_dependencies).
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
---@param opts? table
---@return table
local function new_state(opts)
    local state = {
        source_bufnr = vim.api.nvim_get_current_buf(),
        source_file = vim.api.nvim_buf_get_name(0),
        include_deps = false,
        include_all_deps = false,
        starts_with = false,
        current_word = "",
        default_glob = "*.java",
        fqcn_cache = {},
    }
    if opts then
        state = vim.tbl_extend("force", state, opts)
    end
    return state
end

--- Insert selected static-import items into the invocation source buffer.
local function apply_items(state, items)
    for _, item in ipairs(items) do
        util.add_import_to_buffer(item.name, state.source_bufnr)
    end
end

--- Format a quick-import choice for vim.ui.select.
local function format_select_item(item)
    local class = item.fqcn:match("([^%.]+)$") or item.fqcn
    local pkg = item.fqcn:match("^(.+)%.") or ""
    local member_str = item.member or "*"
    return class .. "." .. member_str .. "  (" .. pkg .. ")"
end

--- Resolve dependency scope from the invocation source buffer.
local function get_scope(state)
    return java_util.is_test_file(state.source_bufnr) and "test" or "main"
end

--- Build quick-import search directories from module sources and preferred deps.
local function get_quick_search_dirs(state)
    local dirs = util.get_module_src_dirs(state.source_bufnr, settings.include_generated_sources)
    vim.list_extend(dirs, util.get_preferred_dep_dirs(get_scope(state), settings))
    return util.dedup_dirs(dirs)
end

--- Open the full picker after a quick-import miss or cancelled selection.
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
    state.current_word = vim.fn.expand("<cword>")

    local dirs = util.get_search_dirs(state, settings)
    if #dirs == 0 then
        vim.notify("[Static Import] No search directories found", vim.log.levels.WARN)
        return
    end

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
    local word = vim.fn.expand("<cword>")
    local state = new_state({ current_word = word })

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
