local java_util = require("utils.java.java-common")
local dep_search = require("modules.java.dependencies-search")
local util = require("modules.java.static-import-explorer.util")

local M = {}

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
        local fqcn = java_util.file_to_fqcn(file)
        if not fqcn then
            vim.notify("[Static Import] Could not determine FQCN", vim.log.levels.WARN)
            return
        end
        local member = nil
        if settings.import_mode == "explicit" then
            member = util.extract_static_member(item.text or "")
            if not member then
                vim.notify("[Static Import] Could not extract member, falling back to wildcard", vim.log.levels.WARN)
            end
        end
        local import_line = util.build_import_line(fqcn, member, settings.import_mode)
        util.add_import_to_buffer(import_line, state.source_bufnr)
    end
end

local function format_item(item)
    local fqcn = java_util.file_to_fqcn(item.file or "")
    local member = util.extract_static_member(item.text or "")
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

---@param settings table
---@param state table
---@param glob? string
function M.open(settings, state, glob)
    local search = util.build_search(state.current_word, state.starts_with)
    Snacks.picker.grep({
        dirs = util.get_search_dirs(state, settings),
        search = search,
        glob = glob or state.default_glob,
        title = "Static Import Search",
        format = format_item,
        confirm = make_confirm(settings, state),
        actions = build_actions(settings, state),
        win = {
            input = { keys = picker_keys },
            list = { keys = picker_keys },
        },
    })
end

return M
