local java_util = require("utils.java.java-common")
local dep_search = require("modules.java.dependencies-search")

local M = {}

local settings = {
    -- "wildcard": import static pkg.Class.*;
    -- "explicit": import static pkg.Class.memberName;
    import_mode = "explicit",
}

local include_deps = false
local include_all_deps = false
local source_bufnr = nil

local function get_module_src_dir()
    local module_root = java_util.get_buffer_project_path()
    if not module_root then
        return nil
    end
    local src_dir = module_root .. "/src"
    if vim.fn.isdirectory(src_dir) == 1 then
        return src_dir
    end
    return nil
end

local function get_search_dirs()
    local dirs = {}
    local src_dir = get_module_src_dir()
    if src_dir then
        table.insert(dirs, src_dir)
    end
    if include_all_deps then
        local dep_dirs = dep_search.get_source_dirs_all()
        if dep_dirs then
            vim.list_extend(dirs, dep_dirs)
        end
    elseif include_deps then
        local dep_dirs = dep_search.get_source_dirs()
        if dep_dirs then
            vim.list_extend(dirs, dep_dirs)
        end
    end
    return dirs
end

--- Extract static member name from a matched grep line.
--- For methods: identifier before '('
--- For fields: ALL_CAPS identifier before '='
local function extract_static_member(text)
    -- Try method: last identifier before (
    local method = text:match("([%a_][%w_]*)%s*%(")
    if method and not vim.tbl_contains({ "if", "for", "while", "switch", "catch" }, method) then
        return method
    end
    -- Try field: ALL_CAPS identifier before =
    local field = text:match("([%u_][%u%d_]+)%s*=")
    if field then
        return field
    end
    return nil
end

local function add_static_import(fqcn, member)
    local import_line
    if member then
        import_line = "import static " .. fqcn .. "." .. member .. ";"
    else
        import_line = "import static " .. fqcn .. ".*;"
    end
    local buf = source_bufnr or vim.api.nvim_get_current_buf()
    local lines = vim.api.nvim_buf_get_lines(buf, 0, -1, false)

    for _, line in ipairs(lines) do
        if line == import_line then
            vim.notify("[Static Import] Already exists: " .. fqcn, vim.log.levels.INFO)
            return
        end
    end

    local insert_after = 0
    for i, line in ipairs(lines) do
        if line:match("^import ") then
            insert_after = i
        elseif insert_after == 0 and line:match("^package ") then
            insert_after = i
        end
    end

    vim.api.nvim_buf_set_lines(buf, insert_after, insert_after, false, { import_line })
    vim.notify("[Static Import] Added: " .. import_line, vim.log.levels.INFO)
end

local function confirm(picker, item)
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
        member = extract_static_member(item.text or "")
        if not member then
            vim.notify("[Static Import] Could not extract member name, falling back to wildcard", vim.log.levels.WARN)
        end
    end
    add_static_import(fqcn, member)
end

local function ensure_deps_loaded(picker, callback)
    if not dep_search.is_loaded() then
        dep_search.load_sources({
            bufnr = source_bufnr,
            on_done = function()
                callback()
            end,
        })
    else
        callback()
    end
end

local function toggle_deps(picker)
    include_deps = not include_deps
    include_all_deps = false
    ensure_deps_loaded(picker, function()
        picker.opts.dirs = get_search_dirs()
        picker:find()
        local label = include_deps and "ON" or "OFF"
        vim.notify("[Static Import] Filtered deps: " .. label, vim.log.levels.INFO)
    end)
end

local function toggle_all_deps(picker)
    include_all_deps = not include_all_deps
    include_deps = false
    ensure_deps_loaded(picker, function()
        picker.opts.dirs = get_search_dirs()
        picker:find()
        local label = include_all_deps and "ON" or "OFF"
        vim.notify("[Static Import] All deps: " .. label, vim.log.levels.INFO)
    end)
end

local default_glob = "*.java"
local starts_with = false
local current_word = ""

local function build_search(word)
    if word == "" then
        return nil
    end
    if word:match("^[A-Z_][A-Z0-9_]*$") then
        -- Static field (ALL_CAPS)
        local suffix = starts_with and "[A-Z0-9_]*[\\s]*=" or "[\\s]*="
        return "static.*[\\s]+" .. word .. suffix
    else
        -- Static method (camelCase)
        local suffix = starts_with and "[a-zA-Z0-9_]*\\(" or "\\("
        return "public[\\s]+static.*[\\s]+" .. word .. suffix
    end
end

-- Forward declaration
local open_picker

local function toggle_starts_with(picker)
    starts_with = not starts_with
    local glob = picker.opts.glob or default_glob
    picker:close()
    open_picker(build_search(current_word), glob)
    local label = starts_with and "starts with" or "full match"
    vim.notify("[Static Import] Match mode: " .. label, vim.log.levels.INFO)
end

local function set_glob(picker)
    vim.ui.input({ prompt = "Class name filter: " }, function(input)
        if not input then
            return
        end
        picker.opts.glob = "*" .. input .. "*.java"
        picker:find()
        vim.notify("[Static Import] Glob: " .. input, vim.log.levels.INFO)
    end)
end

local function clear_glob(picker)
    picker.opts.glob = default_glob
    picker:find()
    vim.notify("[Static Import] Glob reset: " .. default_glob, vim.log.levels.INFO)
end

local actions = {
    toggle_deps = toggle_deps,
    toggle_all_deps = toggle_all_deps,
    set_glob = set_glob,
    clear_glob = clear_glob,
    toggle_starts_with = toggle_starts_with,
}

local keys = {
    ["<C-d>"] = { "toggle_deps", mode = { "n", "i" }, desc = "Toggle filtered dependency sources" },
    ["<C-a>"] = { "toggle_all_deps", mode = { "n", "i" }, desc = "Toggle all dependency sources" },
    ["<C-g>"] = { "set_glob", mode = { "n", "i" }, desc = "Set glob pattern" },
    ["<C-x>"] = { "clear_glob", mode = { "n", "i" }, desc = "Clear glob (reset to *.java)" },
    ["<C-w>"] = { "toggle_starts_with", mode = { "n", "i" }, desc = "Toggle full match / starts with" },
}

open_picker = function(search, glob)
    Snacks.picker.grep({
        dirs = get_search_dirs(),
        search = search,
        glob = glob or default_glob,
        title = "Static Import Search",
        confirm = confirm,
        actions = actions,
        win = {
            input = { keys = keys },
            list = { keys = keys },
        },
    })
end

function M.find()
    source_bufnr = vim.api.nvim_get_current_buf()
    local word = vim.fn.expand("<cword>")
    local dirs = get_search_dirs()
    if #dirs == 0 then
        vim.notify("[Static Import] No search directories found", vim.log.levels.WARN)
        return
    end

    current_word = word
    open_picker(build_search(word))
end

return M
