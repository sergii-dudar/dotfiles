--- Snacks picker wrapper for static-import search results.
---
--- Builds the interactive grep picker used by `static-import-explorer.find()`
--- and by quick-import fallback. It formats grep hits as static import
--- candidates, inserts the selected import into the original source buffer, and
--- exposes search-scope controls:
--- - `<C-d>` toggles filtered dependency sources from `dependencies-search`.
--- - `<C-a>` toggles all dependency sources from `dependencies-search`.
--- - `<C-g>` / `<C-x>` set and clear the Java class-name glob.
--- - `<C-w>` toggles full-match vs starts-with regex mode.
---
--- Public API:
--- - `M.open(settings, state, glob)`: Open the static-import Snacks grep picker.

local dep_search = require("modules.java.dependencies-search")
local util = require("modules.java.static-import-explorer.util")

local M = {}

--- Build the picker confirm callback that inserts the selected static import.
---@param settings table static-import explorer settings
---@param state table invocation state containing source buffer and word
---@return fun(picker: table, item?: table)
local function make_confirm(settings, state)
    return function(picker, item)
        if not item then
            return
        end
        local file = item.file or ""
        if not file:match("%.java$") then
            vim.notify("[Static Import] Not a Java file", vim.log.levels.WARN)
            return
        end
        picker:close()
        local lnum = item.pos and item.pos[1] or nil
        local fqcn = util.file_to_fqcn(file, lnum, state.fqcn_cache)
        -- file_to_fqcn falls back to the bare class name for files without a
        -- `package` line; a dotless FQCN is the default package, which Java cannot
        -- import — reject it rather than insert a broken `import static Class.member;`.
        if not fqcn or not fqcn:find(".", 1, true) then
            vim.notify("[Static Import] Could not determine FQCN", vim.log.levels.WARN)
            return
        end
        local member = nil
        if settings.import_mode == "explicit" then
            member = util.extract_static_member(item.text or "", state.current_word)
            if not member then
                vim.notify("[Static Import] Could not extract member, falling back to wildcard", vim.log.levels.WARN)
            end
        end
        -- Complete the typed prefix to the full member before inserting the import:
        -- the import line goes above the usage and would shift the captured row.
        util.complete_word_in_buffer(state.source_bufnr, state.word_range, state.current_word, member)
        local import_line = util.build_import_line(fqcn, member, settings.import_mode)
        util.add_import_to_buffer(import_line, state.source_bufnr)
    end
end

--- Build the Snacks formatter for displaying class/member search results.
---@param state table invocation state containing the current word and FQCN cache
---@return fun(item: table): table
local function make_format_item(state)
    return function(item)
        local lnum = item.pos and item.pos[1] or nil
        local fqcn = util.file_to_fqcn(item.file or "", lnum, state.fqcn_cache)
        local member = util.extract_static_member(item.text or "", state.current_word)
        if not fqcn then
            local filename = vim.fn.fnamemodify(item.file or "", ":t:r")
            return {
                { filename .. "." .. (member or "?"), "Function" },
            }
        end
        local class_name = fqcn:match("([^%.]+)$") or fqcn
        local pkg = fqcn:match("^(.+)%.") or ""

        local display = class_name .. "." .. (member or "?")
        local ret = {
            { display, "Function" },
        }
        if pkg ~= "" then
            table.insert(ret, { "  " .. pkg, "Comment" })
        end
        return ret
    end
end

--- Ensure dependency source directories are available before refreshing a picker.
---@param state table invocation state containing source buffer
---@param callback fun()
local function ensure_deps_loaded(state, callback)
    if not dep_search.is_loaded() then
        dep_search.load_sources({
            bufnr = state.source_bufnr,
            on_done = callback,
        })
    else
        callback()
    end
end

--- Build custom picker actions for dependency and matching-mode toggles.
--- The filtered/all dependency toggles mutate picker dirs in place because the
--- regex is unchanged. The starts-with toggle reopens the picker because it must
--- rebuild the ripgrep search pattern.
---@param settings table static-import explorer settings
---@param state table invocation state shared with the picker
---@return table<string, fun(picker: table)>
local function build_actions(settings, state)
    return {
        toggle_deps = function(picker)
            state.include_deps = not state.include_deps
            state.include_all_deps = false
            ensure_deps_loaded(state, function()
                picker.opts.dirs = util.get_search_dirs(state, settings)
                picker:find()
                local label = state.include_deps and "ON" or "OFF"
                vim.notify("[Static Import] Filtered deps: " .. label, vim.log.levels.INFO)
            end)
        end,
        toggle_all_deps = function(picker)
            state.include_all_deps = not state.include_all_deps
            state.include_deps = false
            ensure_deps_loaded(state, function()
                picker.opts.dirs = util.get_search_dirs(state, settings)
                picker:find()
                local label = state.include_all_deps and "ON" or "OFF"
                vim.notify("[Static Import] All deps: " .. label, vim.log.levels.INFO)
            end)
        end,
        set_glob = function(picker)
            vim.ui.input({ prompt = "Class name filter: " }, function(input)
                if not input then
                    return
                end
                picker.opts.glob = "*" .. input .. "*.java"
                picker:find()
                vim.notify("[Static Import] Glob: " .. input, vim.log.levels.INFO)
            end)
        end,
        clear_glob = function(picker)
            picker.opts.glob = state.default_glob
            picker:find()
            vim.notify("[Static Import] Glob reset: " .. state.default_glob, vim.log.levels.INFO)
        end,
        toggle_starts_with = function(picker)
            state.starts_with = not state.starts_with
            local glob = picker.opts.glob or state.default_glob
            picker:close()
            M.open(settings, state, glob)
            local label = state.starts_with and "starts with" or "full match"
            vim.notify("[Static Import] Match mode: " .. label, vim.log.levels.INFO)
        end,
    }
end

local picker_keys = {
    ["<C-d>"] = { "toggle_deps", mode = { "n", "i" }, desc = "Toggle filtered dependency sources" },
    ["<C-a>"] = { "toggle_all_deps", mode = { "n", "i" }, desc = "Toggle all dependency sources" },
    ["<C-g>"] = { "set_glob", mode = { "n", "i" }, desc = "Set glob pattern" },
    ["<C-x>"] = { "clear_glob", mode = { "n", "i" }, desc = "Clear glob (reset to *.java)" },
    ["<C-w>"] = { "toggle_starts_with", mode = { "n", "i" }, desc = "Toggle full match / starts with" },
}

--- Open the Snacks picker for the current static-import search state.
--- Starts with module sources plus preferred dependency dirs when available.
--- Users can widen scope with `<C-d>` for filtered dependencies or `<C-a>` for
--- all dependencies. Confirming an item inserts an `import static` line into the
--- source buffer captured in `state.source_bufnr`.
---@param settings table
---@param state table
---@param glob? string
function M.open(settings, state, glob)
    local dirs = util.get_search_dirs(state, settings)
    if #dirs == 0 then
        vim.notify("[Static Import] No search directories found", vim.log.levels.WARN)
        return
    end
    local search = util.build_search(state.current_word, state.starts_with)
    Snacks.picker.grep({
        dirs = dirs,
        search = search,
        glob = glob or state.default_glob,
        title = "Static Import Search",
        format = make_format_item(state),
        confirm = make_confirm(settings, state),
        transform = function(item)
            if item.file == state.source_file then
                return false
            end
            if util.is_excluded_line(item.text or "") then
                return false
            end
        end,
        actions = build_actions(settings, state),
        win = {
            input = { keys = picker_keys },
            list = { keys = picker_keys },
        },
    })
end

return M
