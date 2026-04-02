local java_util = require("utils.java.java-common")
local dep_search = require("modules.java.dependencies-search")

local M = {}

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
function M.get_search_dirs(state)
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
