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

local state = {
    source_bufnr = nil,
    include_deps = false,
    include_all_deps = false,
    starts_with = false,
    current_word = "",
    default_glob = "*.java",
}

local function apply_items(items)
    for _, item in ipairs(items) do
        util.add_import_to_buffer(item.name, state.source_bufnr)
    end
end

local function format_select_item(item)
    local class = item.fqcn:match("([^%.]+)$") or item.fqcn
    local pkg = item.fqcn:match("^(.+)%.") or ""
    local member_str = item.member or "*"
    return class .. "." .. member_str .. "  (" .. pkg .. ")"
end

local function get_scope()
    return java_util.is_test_file(state.source_bufnr) and "test" or "main"
end

local function get_quick_search_dirs()
    local dirs = util.get_module_src_dirs(state.source_bufnr)
    vim.list_extend(dirs, util.get_preferred_dep_dirs(get_scope(), settings))
    return dirs
end

local function fallback_to_find(word)
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

local function on_rg_result(result, pattern, word)
    if result.code ~= 0 or not result.stdout or result.stdout == "" then
        vim.notify("[Static Import] No matches found", vim.log.levels.INFO)
        fallback_to_find(word)
        return
    end

    local items = util.parse_rg_results(result.stdout, settings.import_mode)
    if #items == 0 then
        vim.notify("[Static Import] No valid matches found", vim.log.levels.INFO)
        fallback_to_find(word)
        return
    end

    if #items == 1 and settings.auto_apply_single then
        apply_items(items)
        return
    end

    vim.ui.select(items, {
        prompt = "Static Imports (" .. #items .. ")",
        format_item = format_select_item,
    }, function(choice)
        if choice then
            apply_items({ choice })
        else
            fallback_to_find(word)
        end
    end)
end

local function run_search(word, pattern)
    local search_dirs = get_quick_search_dirs()
    if #search_dirs == 0 then
        vim.notify("[Static Import] No search directories found", vim.log.levels.WARN)
        return
    end

    dd(search_dirs)
    local rg_cmd = { "rg", "-n", "--no-heading", "-e", pattern, "--glob", "*.java" }
    vim.list_extend(rg_cmd, search_dirs)

    vim.system(
        rg_cmd,
        { text = true },
        vim.schedule_wrap(function(result)
            on_rg_result(result, pattern, word)
        end)
    )
end

function M.find()
    state.source_bufnr = vim.api.nvim_get_current_buf()
    state.current_word = vim.fn.expand("<cword>")

    local dirs = util.get_search_dirs(state, settings)
    if #dirs == 0 then
        vim.notify("[Static Import] No search directories found", vim.log.levels.WARN)
        return
    end

    picker.open(settings, state)
end

function M.find_quick()
    state.source_bufnr = vim.api.nvim_get_current_buf()
    local word = vim.fn.expand("<cword>")

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
                run_search(word, pattern)
            end,
        })
    else
        run_search(word, pattern)
    end
end

return M
