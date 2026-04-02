local java_util = require("utils.java.java-common")
local dep_search = require("modules.java.dependencies-search")

local M = {}

--- Convert dependency coordinates to path patterns for substring matching.
--- "org.apache.commons" -> "org/apache/commons/"
--- "org.apache.commons:commons-collections4" -> "org/apache/commons/commons-collections4/"
---@param deps string[]
---@return string[]
function M.to_dep_patterns(deps)
    local patterns = {}
    for _, dep in ipairs(deps) do
        table.insert(patterns, dep:gsub(":", "/"):gsub("%.", "/") .. "/")
    end
    return patterns
end

--- Filter source dirs by dependency patterns (substring match against path).
---@param source_dirs string[]
---@param patterns string[]
---@return string[]
function M.filter_dirs_by_patterns(source_dirs, patterns)
    if #patterns == 0 then
        return {}
    end
    local result = {}
    for _, dir in ipairs(source_dirs) do
        for _, pattern in ipairs(patterns) do
            if dir:find(pattern, 1, true) then
                table.insert(result, dir)
                break
            end
        end
    end
    return result
end

-- Cache for preferred dep dirs, keyed by scope
local preferred_cache = { main = nil, test = nil }

--- Resolve preferred dep entries into directories.
--- Formats:
---   "org.apache.commons" — include all source dirs matching this group
---   "org.apache.commons:commons-lang3" — include specific artifact
---   "!org.apache.commons.commons-compress" — exclude matching dirs
---   "org.assertj:assertj-core#org/assertj/core/util;org/assertj/core/api" — include only specific subdirs
---@param entries string[]
---@param all_dep_dirs string[]
---@return string[]
local function resolve_preferred_entries(entries, all_dep_dirs)
    local dep_coords = {}
    local dep_with_subpaths = {} -- { coord, subpaths[] }
    local exclude_coords = {}
    local dirs = {}

    for _, entry in ipairs(entries) do
        if entry:match("^!") then
            table.insert(exclude_coords, entry:sub(2))
        elseif entry:find("#", 1, true) then
            local coord, subpaths_str = entry:match("^(.-)#(.+)$")
            if coord and subpaths_str then
                local subpaths = vim.split(subpaths_str, ";", { trimempty = true })
                table.insert(dep_with_subpaths, { coord = coord, subpaths = subpaths })
            end
        else
            table.insert(dep_coords, entry)
        end
    end

    -- Resolve full dep dirs
    if #dep_coords > 0 then
        local patterns = M.to_dep_patterns(dep_coords)
        vim.list_extend(dirs, M.filter_dirs_by_patterns(all_dep_dirs, patterns))
    end

    -- Resolve deps with subpath restrictions
    for _, item in ipairs(dep_with_subpaths) do
        local patterns = M.to_dep_patterns({ item.coord })
        local matched_dirs = M.filter_dirs_by_patterns(all_dep_dirs, patterns)
        for _, dep_dir in ipairs(matched_dirs) do
            for _, subpath in ipairs(item.subpaths) do
                local full = dep_dir .. "/" .. subpath
                local matches = vim.fn.glob(full, false, true)
                for _, match in ipairs(matches) do
                    if vim.fn.isdirectory(match) == 1 then
                        table.insert(dirs, match)
                    end
                end
            end
        end
    end

    -- Remove excluded dirs
    if #exclude_coords > 0 then
        local exclude_patterns = M.to_dep_patterns(exclude_coords)
        dirs = vim.tbl_filter(function(dir)
            for _, pattern in ipairs(exclude_patterns) do
                if dir:find(pattern, 1, true) then
                    return false
                end
            end
            return true
        end, dirs)
    end

    return dirs
end

--- Get cached preferred dep dirs for a scope.
---@param scope "main"|"test"
---@param settings { preferred_deps_main: string[], preferred_deps_test: string[] }
---@return string[]
function M.get_preferred_dep_dirs(scope, settings)
    if preferred_cache[scope] then
        return preferred_cache[scope]
    end
    if not dep_search.is_loaded() then
        return {}
    end
    local preferred = scope == "test" and settings.preferred_deps_test or settings.preferred_deps_main
    if not preferred or #preferred == 0 then
        preferred_cache[scope] = {}
        return {}
    end
    local all_dep_dirs = dep_search.get_source_dirs_all(scope) or {}
    preferred_cache[scope] = resolve_preferred_entries(preferred, all_dep_dirs)
    return preferred_cache[scope]
end

function M.clear_preferred_cache()
    preferred_cache = { main = nil, test = nil }
end

--- Returns source dirs based on buffer context:
--- main file -> src/main/java only
--- test file -> src/main/java + src/test/java
---@param bufnr? integer
---@return string[]
function M.get_module_src_dirs(bufnr)
    local module_root = java_util.get_buffer_project_path(bufnr)
    if not module_root then
        return {}
    end
    local dirs = {}
    local main_dir = module_root .. "/src/main/java"
    if vim.fn.isdirectory(main_dir) == 1 then
        table.insert(dirs, main_dir)
    end
    if java_util.is_test_file(bufnr) then
        local test_dir = module_root .. "/src/test/java"
        if vim.fn.isdirectory(test_dir) == 1 then
            table.insert(dirs, test_dir)
        end
    end
    return dirs
end

---@param state { include_deps: boolean, include_all_deps: boolean, source_bufnr?: integer }
---@param settings? { preferred_deps_main?: string[], preferred_deps_test?: string[] }
function M.get_search_dirs(state, settings)
    local dirs = M.get_module_src_dirs(state.source_bufnr)
    if #dirs == 0 then
        -- fallback: try src/ directly
        local module_root = java_util.get_buffer_project_path(state.source_bufnr)
        if module_root then
            local src_dir = module_root .. "/src"
            if vim.fn.isdirectory(src_dir) == 1 then
                table.insert(dirs, src_dir)
            end
        end
    end
    local scope = java_util.is_test_file(state.source_bufnr) and "test" or "main"

    -- Include preferred deps by default (if deps are loaded)
    if settings and dep_search.is_loaded() then
        vim.list_extend(dirs, M.get_preferred_dep_dirs(scope, settings))
    end

    if state.include_all_deps then
        local dep_dirs = dep_search.get_source_dirs_all(scope)
        if dep_dirs then
            vim.list_extend(dirs, dep_dirs)
        end
    elseif state.include_deps then
        local dep_dirs = dep_search.get_source_dirs(scope)
        if dep_dirs then
            vim.list_extend(dirs, dep_dirs)
        end
    end
    return dirs
end

--- Extract static member name from a matched grep line.
--- For methods: identifier before '('
--- For fields: ALL_CAPS identifier before '='
function M.extract_static_member(text)
    local method = text:match("([%a_][%w_]*)%s*%(")
    if method and not vim.tbl_contains({ "if", "for", "while", "switch", "catch" }, method) then
        return method
    end
    local field = text:match("([%u_][%u%d_]+)%s*=")
    if field then
        return field
    end
    return nil
end

---@param fqcn string
---@param member? string
---@param import_mode string "wildcard"|"explicit"
function M.build_import_line(fqcn, member, import_mode)
    if import_mode == "explicit" and member then
        return "import static " .. fqcn .. "." .. member .. ";"
    end
    return "import static " .. fqcn .. ".*;"
end

--- Add a static import line to the given buffer.
---@param import_line string
---@param bufnr integer
function M.add_import_to_buffer(import_line, bufnr)
    local lines = vim.api.nvim_buf_get_lines(bufnr, 0, -1, false)

    for _, line in ipairs(lines) do
        if line == import_line then
            vim.notify("[Static Import] Already exists", vim.log.levels.INFO)
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

    vim.api.nvim_buf_set_lines(bufnr, insert_after, insert_after, false, { import_line })
    vim.notify("[Static Import] Added: " .. import_line, vim.log.levels.INFO)
end

---@param word string
---@param starts_with boolean
function M.build_search(word, starts_with)
    if word == "" then
        return nil
    end
    if word:match("^[A-Z_][A-Z0-9_]*$") then
        -- Static field (ALL_CAPS) — no public/static required (covers interface fields)
        local suffix = starts_with and "[A-Z0-9_]*[\\s]*=" or "[\\s]*="
        -- return "public[\\s]+static.*[\\s]+" .. word .. suffix -- not supported statics declared in `interface`
        return "\\w+[\\s]+" .. word .. suffix
    else
        -- Static method (camelCase) — requires static, public optional (covers interface methods)
        local suffix = starts_with and "[a-zA-Z0-9_]*\\(" or "\\("
        -- return "public[\\s]+static.*[\\s]+" .. word .. suffix -- in `interface`
        return "static[\\s]+.*[\\s]+" .. word .. suffix
    end
end

--- Derive FQCN from Java file path (no I/O, uses path structure).
--- Looks for src/main/java/ or src/test/java/ prefix to extract package.
---@param file string
---@return string|nil fqcn, string|nil class_name
function M.fqcn_from_path(file)
    local rel = file:match("src/main/java/(.+)%.java$") or file:match("src/test/java/(.+)%.java$")
    if not rel then
        -- fallback: try any path ending with .java after a known java source root pattern
        rel = file:match("/java/(.+)%.java$")
    end
    if rel then
        local fqcn = rel:gsub("/", ".")
        local class_name = fqcn:match("([^%.]+)$")
        return fqcn, class_name
    end
    local class_name = vim.fn.fnamemodify(file, ":t:r")
    return nil, class_name
end

--- Parse rg output lines into deduplicated import items.
---@param stdout string
---@param import_mode string
---@return { name: string, fqcn: string, member: string|nil }[]
function M.parse_rg_results(stdout, import_mode)
    local lines = vim.split(stdout, "\n", { trimempty = true })
    local seen = {}
    local items = {}

    for _, line in ipairs(lines) do
        local file, text = line:match("^(.-):%d+:(.*)")
        if file and text then
            local fqcn = java_util.file_to_fqcn(file)
            if fqcn then
                local member = M.extract_static_member(text)
                local import_str = M.build_import_line(fqcn, member, import_mode)
                if not seen[import_str] then
                    seen[import_str] = true
                    table.insert(items, {
                        name = import_str,
                        fqcn = fqcn,
                        member = member,
                    })
                end
            end
        end
    end

    return items
end

return M
