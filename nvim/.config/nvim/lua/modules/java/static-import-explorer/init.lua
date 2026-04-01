local util = require("modules.java.static-import-explorer.util")
local picker = require("modules.java.static-import-explorer.picker")

local M = {}

local settings = {
    -- "wildcard": import static pkg.Class.*;
    -- "explicit": import static pkg.Class.memberName;
    import_mode = "explicit",
    -- auto-import without showing multiselect when only one result found
    auto_apply_single = true,
    -- fallback to find() when quick has no results or user cancels select
    fallback_to_find = true,
}

local state = {
    source_bufnr = nil,
    include_deps = false,
    include_all_deps = false,
    starts_with = false,
    current_word = "",
    default_glob = "*.java",
}

function M.find()
    state.source_bufnr = vim.api.nvim_get_current_buf()
    state.current_word = vim.fn.expand("<cword>")

    local dirs = util.get_search_dirs(state)
    if #dirs == 0 then
        vim.notify("[Static Import] No search directories found", vim.log.levels.WARN)
        return
    end

    picker.open(settings, state)
end

local function apply_items(items)
    for _, item in ipairs(items) do
        util.add_import_to_buffer(item.name, state.source_bufnr)
    end
end

function M.find_quick()
    state.source_bufnr = vim.api.nvim_get_current_buf()
    local word = vim.fn.expand("<cword>")

    if word == "" then
        vim.notify("[Static Import] No word under cursor", vim.log.levels.WARN)
        return
    end

    local src_dir = util.get_module_src_dir()
    if not src_dir then
        vim.notify("[Static Import] No src/ directory found", vim.log.levels.WARN)
        return
    end

    -- local pattern = util.build_search(word, state.starts_with)
    local pattern = util.build_search(word, true)
    if not pattern then
        return
    end

    local function fallback_to_find()
        if settings.fallback_to_find then
            state.current_word = word
            state.include_all_deps = true
            state.include_deps = false
            picker.open(settings, state)
        end
    end

    vim.system(
        { "rg", "-n", "--no-heading", "-e", pattern, "--glob", "*.java", src_dir },
        { text = true },
        vim.schedule_wrap(function(result)
            if result.code ~= 0 or not result.stdout or result.stdout == "" then
                vim.notify("[Static Import] No matches found", vim.log.levels.INFO)
                fallback_to_find()
                return
            end

            local items = util.parse_rg_results(result.stdout, settings.import_mode)
            if #items == 0 then
                vim.notify("[Static Import] No valid matches found", vim.log.levels.INFO)
                fallback_to_find()
                return
            end

            if #items == 1 and settings.auto_apply_single then
                apply_items(items)
                return
            end

            vim.ui.select(items, {
                prompt = "Static Imports (" .. #items .. ")",
                format_item = function(item)
                    local class = item.fqcn:match("([^%.]+)$") or item.fqcn
                    local pkg = item.fqcn:match("^(.+)%.") or ""
                    local member_str = item.member or "*"
                    return class .. "." .. member_str .. "  (" .. pkg .. ")"
                end,
            }, function(choice)
                if choice then
                    apply_items({ choice })
                else
                    fallback_to_find()
                end
            end)
        end)
    )
end

return M