--- Utility helpers for Java static-import discovery and insertion.
---
--- This module contains the pure-ish pieces used by both quick import and the
--- picker: dependency-coordinate matching, source directory composition,
--- ripgrep regex construction, grep-result parsing, static member extraction,
--- FQCN lookup/caching, and import-line insertion.
---
--- Public API:
--- - `M.to_dep_patterns(deps)`: Convert dependency coordinates to slash-form path patterns.
--- - `M.filter_dirs_by_patterns(source_dirs, patterns)`: Keep dependency dirs matching any pattern.
--- - `M.dedup_dirs(dirs)`: Deduplicate directory lists while preserving order.
--- - `M.get_preferred_dep_dirs(scope, settings)`: Resolve preferred dependency entries for a scope.
--- - `M.clear_preferred_cache()`: Clear preferred dependency dir cache after dep reload/reset.
--- - `M.get_module_src_dirs(bufnr, include_generated_sources)`: Return Java source roots for a buffer module.
--- - `M.get_search_dirs(state, settings)`: Compose project, preferred, filtered, and all dependency dirs.
--- - `M.is_excluded_line(text)`: Detect grep lines that should not become import candidates.
--- - `M.extract_static_member(text, word)`: Extract a static field, enum constant, or method name from a line.
--- - `M.build_import_line(fqcn, member, import_mode)`: Build an `import static` line.
--- - `M.add_import_to_buffer(import_line, bufnr)`: Insert a static import into a Java buffer.
--- - `M.cword_range()`: Range + text of the identifier under the cursor in the current window.
--- - `M.complete_word_in_buffer(bufnr, range, expected, member)`: Complete a prefix to the imported member name.
--- - `M.build_search(word, starts_with)`: Build the ripgrep regex for the word under cursor.
--- - `M.file_to_fqcn(file, line_num, cache)`: Resolve and optionally cache Java file FQCNs.
--- - `M.fqcn_from_path(file)`: Derive an FQCN from a Java source path without reading the file.
--- - `M.parse_rg_results(...)`: Parse ripgrep output into deduplicated import candidates.

local java_util = require("utils.java.java-common")
local dep_search = require("modules.java.dependencies-search")

local M = {}

--- Convert dependency coordinates to path patterns for substring matching.
--- "org.apache.commons" -> "org/apache/commons/"
--- "org.apache.commons:commons-collections4" -> "org/apache/commons/commons-collections4/"
--- The resulting slash-form patterns are matched against Maven paths directly
--- and against Gradle paths after `dependencies-search.coord_match_path()`
--- normalizes the dotted Gradle group segment.
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
--- Supports both Maven and Gradle dependency cache paths by normalizing each
--- source dir through `dependencies-search.coord_match_path()` before matching.
---@param source_dirs string[]
---@param patterns string[]
---@return string[]
function M.filter_dirs_by_patterns(source_dirs, patterns)
    if #patterns == 0 then
        return {}
    end
    local result = {}
    for _, dir in ipairs(source_dirs) do
        -- Normalize once per dir: Gradle cache paths embed the group as a single
        -- dotted segment (.../files-2.1/org.assertj/...), so slash-form patterns
        -- (org/assertj/...) won't match without this. No-op for Maven dirs.
        local match_path = dep_search.coord_match_path(dir)
        for _, pattern in ipairs(patterns) do
            if match_path:find(pattern, 1, true) then
                table.insert(result, dir)
                break
            end
        end
    end
    return result
end

--- Return directories without duplicates while preserving their first-seen order.
---@param dirs string[]
---@return string[]
function M.dedup_dirs(dirs)
    local result = {}
    local seen = {}
    for _, dir in ipairs(dirs or {}) do
        if dir ~= "" and not seen[dir] then
            seen[dir] = true
            table.insert(result, dir)
        end
    end
    return result
end

-- Cache for preferred dep dirs, keyed by scope
local preferred_cache = { main = nil, test = nil }

--- Resolve preferred dependency entries into source directories.
--- Entry formats:
--- - `"org.apache.commons"`: include all source dirs matching this group.
--- - `"org.apache.commons:commons-lang3"`: include one artifact.
--- - `"!org.apache.commons.commons-compress"`: exclude matching dirs.
--- - `"org.assertj:assertj-core#org/assertj/core/util;org/assertj/core/api"`:
---   include only specific subdirs inside the matched artifact source dir.
--- Matching works for Maven and Gradle cache paths via coordinate normalization.
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
            local match_path = dep_search.coord_match_path(dir)
            for _, pattern in ipairs(exclude_patterns) do
                if match_path:find(pattern, 1, true) then
                    return false
                end
            end
            return true
        end, dirs)
    end

    return M.dedup_dirs(dirs)
end

--- Get cached preferred dep dirs for a scope.
--- Preferred dirs are resolved from `dependencies-search.get_source_dirs_all()`
--- so they can include libraries that are outside the default filtered dep set.
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

--- Clear cached preferred dependency directories after dependency cache reset.
function M.clear_preferred_cache()
    preferred_cache = { main = nil, test = nil }
end

--- Add an existing directory to a list.
--- Missing directories are ignored so callers can list Maven/Gradle generated
--- source conventions without checking each path first.
---@param dirs string[]
---@param dir string
local function add_existing_dir(dirs, dir)
    if vim.fn.isdirectory(dir) == 1 then
        table.insert(dirs, dir)
    end
end

--- Returns generated Java source directories for a module root.
--- Includes Maven `target/generated-sources`, Gradle `build/generated/sources`,
--- and Maven test generated sources when the invocation buffer is a test file.
---@param module_root string
---@param include_test boolean
---@return string[]
local function get_generated_src_dirs(module_root, include_test)
    local dirs = {}

    add_existing_dir(dirs, module_root .. "/target/generated-sources")
    add_existing_dir(dirs, module_root .. "/build/generated/sources")

    if include_test then
        add_existing_dir(dirs, module_root .. "/target/generated-test-sources")
    end

    return M.dedup_dirs(dirs)
end

--- Return Java source dirs based on buffer context.
--- Main files search `src/main/java` plus generated main sources when enabled.
--- Test files search `src/main/java`, `src/test/java`, and generated main/test
--- sources when enabled.
---@param bufnr? integer
---@param include_generated_sources? boolean defaults to true
---@return string[]
function M.get_module_src_dirs(bufnr, include_generated_sources)
    local module_root = java_util.get_buffer_project_path(bufnr)
    if not module_root then
        return {}
    end
    local dirs = {}
    local main_dir = module_root .. "/src/main/java"
    if vim.fn.isdirectory(main_dir) == 1 then
        table.insert(dirs, main_dir)
    end
    local is_test = java_util.is_test_file(bufnr)
    if include_generated_sources ~= false then
        vim.list_extend(dirs, get_generated_src_dirs(module_root, is_test))
    end
    if is_test then
        local test_dir = module_root .. "/src/test/java"
        if vim.fn.isdirectory(test_dir) == 1 then
            table.insert(dirs, test_dir)
        end
    end
    return M.dedup_dirs(dirs)
end

--- Build the directory list for the current static-import search state.
--- Always starts with current module Java source dirs. Preferred dependency dirs
--- are added when dependency sources are loaded. Picker toggles then optionally
--- append filtered dependency dirs (`include_deps`) or every dependency dir
--- (`include_all_deps`).
---@param state { include_deps: boolean, include_all_deps: boolean, source_bufnr?: integer }
---@param settings? { preferred_deps_main?: string[], preferred_deps_test?: string[] }
---@return string[] dirs deduplicated search directories
function M.get_search_dirs(state, settings)
    local include_generated_sources = not settings or settings.include_generated_sources ~= false
    local dirs = M.get_module_src_dirs(state.source_bufnr, include_generated_sources)
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
    return M.dedup_dirs(dirs)
end

--- Return true when a grep match line is noise rather than an importable member:
--- a `return` statement, or a `private`/`protected` declaration. Matched as whole
--- words (frontier pattern) so identifiers like `returnValue`/`protectedName` are kept.
--- Single source of truth shared by the picker `transform` and `parse_rg_results`.
---@param text string
---@return boolean
function M.is_excluded_line(text)
    return text:match("%f[%w]return%f[%W]") ~= nil
        or text:match("%f[%w]private%f[%W]") ~= nil
        or text:match("%f[%w]protected%f[%W]") ~= nil
end

--- Extract static member name from a matched grep line.
--- For fields: ALL_CAPS identifier before '=' or ';' (checked first to avoid matching method calls in initializers)
--- For enum constants: ALL_CAPS at start of trimmed line before ',' ';' or '(' (constructor args)
--- For methods: identifier before '('
---@param text string grep match line
---@param word? string the searched identifier — resolves the correct member in inline declarations like "NONE, DEBTOR, CREDITOR;"
---@return string|nil member extracted import member name
function M.extract_static_member(text, word)
    -- When the searched word is known, find it directly first. Without this, an inline enum
    -- like "    NONE, DEBTOR, CREDITOR;" returns CREDITOR (matched by the field pattern below)
    -- regardless of whether the user searched for NONE, DEBTOR, or CREDITOR.
    if word and word ~= "" then
        local escaped = vim.pesc(word)
        local found = text:match("^%s*(" .. escaped .. "[%u%d_]*)%s*[,;({=]")
            or text:match("[^%u%d_](" .. escaped .. "[%u%d_]*)%s*[,;({=]")
            -- Trailing-whitespace-only: last enum constant with no terminator on this line.
            or text:match("^%s*(" .. escaped .. "[%u%d_]*)%s*$")
            or text:match("[^%u%d_](" .. escaped .. "[%u%d_]*)%s*$")
        if found then
            return found
        end
    end
    -- Field: ALL_CAPS with explicit type prefix (e.g. `static final Instant FOO = ...` or `WireMockServer BAR;`)
    local field = text:match("([%u_][%u%d_]+)%s*[=;]")
    if field then
        return field
    end
    -- Enum constant: ALL_CAPS at start of trimmed line, terminated by , ; ( (constructor) or { (body)
    local enum_const = text:match("%s*([%u_][%u%d_]+)%s*[,;({]")
    if enum_const then
        return enum_const
    end
    -- Method: camelCase identifier before (
    local method = text:match("([%a_][%w_]*)%s*%(")
    if method and not vim.tbl_contains({ "if", "for", "while", "switch", "catch" }, method) then
        return method
    end
    return nil
end

--- Build an import statement for the selected class/member.
---@param fqcn string
---@param member? string
---@param import_mode string "wildcard"|"explicit"
---@return string
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
    if not bufnr or not vim.api.nvim_buf_is_valid(bufnr) then
        vim.notify("[Static Import] Source buffer is no longer available", vim.log.levels.WARN)
        return
    end

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

--- Return the range and text of the identifier under the cursor in the current window.
--- Mirrors Vim's `<cword>` semantics: when the cursor is on a keyword char the whole
--- run is taken; otherwise the next keyword run on the line is used. Keyword chars are
--- `[%w_]` (Java identifier chars). Coordinates are 0-indexed, `col_end` exclusive, ready
--- for `nvim_buf_get_text` / `nvim_buf_set_text`.
---@return { row: integer, col_start: integer, col_end: integer }|nil range
---@return string|nil word
function M.cword_range()
    local pos = vim.api.nvim_win_get_cursor(0)
    local row = pos[1] - 1 -- 0-indexed row
    local line = vim.api.nvim_get_current_line()
    local n = #line
    if n == 0 then
        return nil, nil
    end

    local function is_kw(i)
        return line:sub(i, i):match("[%w_]") ~= nil
    end

    local ci = pos[2] + 1 -- 1-indexed byte column into the line
    if ci > n then
        ci = n
    end
    -- Cursor not on a keyword char: advance to the start of the next keyword run.
    if not is_kw(ci) then
        local j = ci
        while j <= n and not is_kw(j) do
            j = j + 1
        end
        if j > n then
            return nil, nil
        end
        ci = j
    end

    local s = ci
    while s > 1 and is_kw(s - 1) do
        s = s - 1
    end
    local e = ci
    while e < n and is_kw(e + 1) do
        e = e + 1
    end

    return { row = row, col_start = s - 1, col_end = e }, line:sub(s, e)
end

--- Complete a typed prefix under the cursor to the full imported member name.
--- After a static import is added for a prefix search (cursor on `SIGNE`, member
--- `SIGNED_NUM`), this rewrites the identifier in place. It is a no-op when there is
--- no member, the text already equals the member, or the captured range no longer
--- holds the originally-typed word (buffer edited / cursor moved since invocation).
--- Call this BEFORE `add_import_to_buffer` so the captured row stays valid — the
--- import line is inserted above the usage and would otherwise shift it down.
---@param bufnr integer
---@param range { row: integer, col_start: integer, col_end: integer }|nil
---@param expected string the word originally under the cursor
---@param member string|nil the full member name to write
function M.complete_word_in_buffer(bufnr, range, expected, member)
    if not range or not member or member == "" or member == expected then
        return
    end
    if not bufnr or not vim.api.nvim_buf_is_valid(bufnr) then
        return
    end
    local ok, cur = pcall(vim.api.nvim_buf_get_text, bufnr, range.row, range.col_start, range.row, range.col_end, {})
    if not ok or cur[1] ~= expected then
        return
    end
    vim.api.nvim_buf_set_text(bufnr, range.row, range.col_start, range.row, range.col_end, { member })
end

--- Build a ripgrep regex for a possible static member.
--- ALL_CAPS words use field and enum-constant patterns. Other words use a
--- same-line `static ... word(` method pattern. `starts_with` widens the match
--- for prefix search in quick import and picker toggle mode.
---@param word string
---@param starts_with boolean
---@return string|nil pattern ripgrep expression, or nil for an empty word
function M.build_search(word, starts_with)
    if word == "" then
        return nil
    end
    if word:match("^[A-Z_][A-Z0-9_]*$") then
        -- Static field (ALL_CAPS) — no public/static required (covers interface fields).
        -- Also incidentally catches inline enum constants when terminator is ',' or ';'.
        local suffix = starts_with and "[A-Z0-9_]*[\\s]*[,=;]" or "[\\s]*[,=;]"
        local field_pattern = "[^\\s]+[\\s]+" .. word .. suffix
        -- Enum constant — at start of trimmed line OR after a comma; terminated by , ; ( { or end of line.
        -- End-of-line terminator covers the last constant in vertical layout (";" / "}" on next line).
        local enum_tail = starts_with and "[A-Z0-9_]*" or ""
        local enum_pattern = "(?:^|,)[\\s]*" .. word .. enum_tail .. "[\\s]*(?:[,;({]|$)"
        return field_pattern .. "|" .. enum_pattern
    else
        -- Static method (camelCase) — requires static, public optional (covers interface methods).
        -- NOTE: rg matches per line, so `static` and the method name must be on the same line.
        -- Signatures split across lines (e.g. `public static <T> T\n    mock(...)`) are missed by
        -- design — matching multi-line would require rg -U and inflate false positives.
        local suffix = starts_with and "[a-zA-Z0-9_]*\\(" or "\\("
        return "static[\\s]+.*[\\s]+" .. word .. suffix
    end
end

--- Resolve a Java file to FQCN with an optional per-invocation cache.
---@param file string
---@param line_num? integer
---@param cache? table<string, string|false>
---@return string|nil
function M.file_to_fqcn(file, line_num, cache)
    if not cache then
        return java_util.file_to_fqcn(file, line_num)
    end

    local key = file .. ":" .. tostring(line_num or "")
    local cached = cache[key]
    if cached ~= nil then
        return cached or nil
    end

    local fqcn = java_util.file_to_fqcn(file, line_num)
    cache[key] = fqcn or false
    return fqcn
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
---@param word? string the searched identifier — passed to extract_static_member to resolve inline declarations
---@param fqcn_cache? table<string, string|false>
---@param source_file? string file to exclude from import candidates
---@return { name: string, fqcn: string, member: string|nil }[]
function M.parse_rg_results(stdout, import_mode, word, fqcn_cache, source_file)
    local lines = vim.split(stdout, "\n", { trimempty = true })
    local seen = {}
    local items = {}

    for _, line in ipairs(lines) do
        local file, lnum_str, text = line:match("^(.-):(%d+):(.*)")
        if file and text and file ~= source_file and not M.is_excluded_line(text) then
            local fqcn = M.file_to_fqcn(file, tonumber(lnum_str), fqcn_cache)
            -- Require a package: file_to_fqcn falls back to the bare class name for
            -- files without a `package` line, and Java forbids importing from the
            -- default package, so a dotless FQCN can never form a valid static import.
            if fqcn and fqcn:find(".", 1, true) then
                local member = M.extract_static_member(text, word)
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

    -- Rank exact `member == word` matches above prefix matches; alphabetical within each tier.
    if word and word ~= "" then
        table.sort(items, function(a, b)
            local a_exact = a.member == word
            local b_exact = b.member == word
            if a_exact ~= b_exact then
                return a_exact
            end
            return (a.member or "") < (b.member or "")
        end)
    end

    return items
end

return M
