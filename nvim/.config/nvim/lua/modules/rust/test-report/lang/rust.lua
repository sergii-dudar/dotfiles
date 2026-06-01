-- Rust language adapter for test-report.
-- Provides:
--   * parse_results: delegates to nextest-xml.lua
--   * id_to_file: maps "<binary_id>::<module_path>" container IDs to source files
--                 via a per-workspace treesitter index of #[test] functions.
--   * find_test_positions: treesitter search inside a file for #[test]/etc.
--   * extract_error_line: regex over Rust panic stacktrace
--   * id_to_display: splits "<binary_id>::<mod>::..." -> { group, container, member }
--
-- Test ID convention (synced with nextest-xml):
--   <binary_id>::<module_path>#<fn_name>     (nested test)
--   <binary_id>#<fn_name>                    (test at binary root)
--
-- File-to-container resolution rules (Cargo convention):
--   src/lib.rs                -> binary_id = <crate_name>
--   src/main.rs               -> binary_id = <crate_name>
--   src/bin/<n>.rs            -> binary_id = <crate_name>$<n>
--   src/bin/<n>/main.rs       -> binary_id = <crate_name>$<n>
--   tests/<n>.rs              -> binary_id = <crate_name>::<n>
--   tests/<n>/main.rs         -> binary_id = <crate_name>::<n>
-- Module path inside a file:
--   walk up `mod <name> { ... }` nesting tree-sitter-style.

local nextest_xml = require("modules.rust.test-report.nextest-xml")
local log = require("utils.logging-util").new({
    name = "test-report-rust",
    filename = "test-report.log",
    level = vim.log.levels.DEBUG,
})

---@type test_report.LangAdapter
local M = {
    group_separator = "::",
    diagnostic_source = "cargo-test",
    trouble_source = "cargo_test_diagnostics",
}

-- workspace_root -> {
--   container_files    = { [container_id] = file_path },
--   positions_by_file  = { [file_path] = { positions = {...}, container_line = N|nil } },
--   built_at           = hrtime,
-- }
local index_cache = {}

function M.clear_cache()
    index_cache = {}
end

---@param dirs string[]
---@return table<string, test_report.TestResult>
function M.parse_results(dirs)
    return nextest_xml.parse_results(dirs)
end

---@return string|nil
local function cargo_workspace_root_from_cwd(cwd)
    local cmd = string.format("cd %s 2>/dev/null && cargo metadata --no-deps --format-version 1 2>/dev/null", cwd)
    local handle = io.popen(cmd)
    if not handle then
        return nil
    end
    local out = handle:read("*a")
    handle:close()
    local ok, meta = pcall(vim.fn.json_decode, out)
    if not ok or type(meta) ~= "table" then
        return nil
    end
    return meta.workspace_root
end

--- Derive workspace root from a report dir like .../target/nextest/<profile>/
---@param report_dir string|nil
---@return string|nil
local function workspace_root_from_report_dir(report_dir)
    if not report_dir then
        return nil
    end
    local root = report_dir:match("^(.+)/target/nextest/[^/]+/?$")
    if root then
        return root
    end
    root = report_dir:match("^(.+)/target/nextest/?$")
    return root
end

---@return string|nil
local function resolve_workspace_root(report_dir)
    return workspace_root_from_report_dir(report_dir) or cargo_workspace_root_from_cwd(vim.fn.getcwd())
end

---@param workspace_root string
---@return table[]  cargo metadata packages
local function cargo_metadata_packages(workspace_root)
    local cmd = string.format(
        "cd %s 2>/dev/null && cargo metadata --no-deps --format-version 1 2>/dev/null",
        vim.fn.shellescape(workspace_root)
    )
    local handle = io.popen(cmd)
    if not handle then
        return {}
    end
    local out = handle:read("*a")
    handle:close()
    local ok, meta = pcall(vim.fn.json_decode, out)
    if not ok or type(meta) ~= "table" or not meta.packages then
        return {}
    end
    return meta.packages
end

--- Compute the binary_id for a given source file inside a package.
---@param package_name_snake string  e.g. "my_crate"
---@param target_kind string         "lib", "bin", "test", "example", "bench"
---@param target_name string         For bin/test/example/bench: the user-facing target name
---@return string|nil
local function compute_binary_id(package_name_snake, target_kind, target_name)
    if target_kind == "lib" then
        return package_name_snake
    end
    if target_kind == "bin" then
        if target_name == package_name_snake then
            return package_name_snake -- default bin (main.rs) shares package name
        end
        return package_name_snake .. "$" .. target_name
    end
    if target_kind == "test" then
        return package_name_snake .. "::" .. target_name
    end
    -- bench / example: not directly testable via cargo nextest
    return nil
end

local _test_query
local function test_query()
    if _test_query then
        return _test_query
    end
    _test_query = vim.treesitter.query.parse(
        "rust",
        [[
        (mod_item
          name: (identifier) @mod.name
        ) @mod.definition

        (function_item
          name: (identifier) @fn.name
        ) @fn.definition

        (attribute_item
          (attribute (identifier) @attr.name)
        ) @attr.node

        (attribute_item
          (attribute (scoped_identifier) @attr.scoped)
        ) @attr.scoped_node
    ]]
    )
    return _test_query
end

-- Test attribute identifiers we consider as marking a test fn.
local test_attr_names = {
    test = true,
    rstest = true,
    ["tokio::test"] = true,
    ["async_std::test"] = true,
    ["test_case"] = true,
}

--- Parse a Rust source file and yield:
---   * a list of test fns with their module paths inside the file:
---     { { mod_path = "tests::inner", fn_name = "deep_test", line = N }, ... }
---   * the start line of the outermost cfg(test) module (used as container_line for signs)
---@param file_path string
---@param opts? test_report.FindOpts
---@return { mod_path: string, fn_name: string, line: integer }[] tests
---@return integer|nil container_line
local function parse_test_fns(file_path, opts)
    local silent = not opts or opts.silent ~= false

    local bufnr = vim.fn.bufadd(file_path)
    if not vim.api.nvim_buf_is_loaded(bufnr) then
        if silent then
            pcall(vim.cmd, "noautocmd call bufload(" .. bufnr .. ")")
        else
            vim.fn.bufload(bufnr)
        end
    end

    local ok, parser = pcall(vim.treesitter.get_parser, bufnr, "rust")
    if not ok or not parser then
        log.error("rust treesitter parser failed: " .. tostring(parser))
        return {}, nil
    end
    local tree = parser:parse()[1]
    if not tree then
        return {}, nil
    end
    local root = tree:root()

    -- Walk the AST manually so we can track the mod stack.
    local results = {}
    local container_line = nil

    local function node_text(node)
        return vim.treesitter.get_node_text(node, bufnr)
    end

    -- Returns true if any preceding sibling attribute marks fn as a test.
    local function fn_has_test_attribute(fn_node)
        local prev = fn_node:prev_named_sibling()
        while prev and prev:type() == "attribute_item" do
            -- Find attribute identifier or scoped_identifier inside
            for child in prev:iter_children() do
                if child:type() == "attribute" then
                    for cc in child:iter_children() do
                        local ct = cc:type()
                        if ct == "identifier" or ct == "scoped_identifier" then
                            local name = node_text(cc)
                            if test_attr_names[name] then
                                return true
                            end
                            -- Match any attribute ending in "::test" (rare custom test macros).
                            if name:match("::test$") then
                                return true
                            end
                        end
                    end
                end
            end
            prev = prev:prev_named_sibling()
        end
        return false
    end

    local function walk(node, mod_stack)
        local t = node:type()
        if t == "mod_item" then
            local name_node
            for child in node:iter_children() do
                if child:type() == "identifier" then
                    name_node = child
                    break
                end
            end
            local mod_name = name_node and node_text(name_node) or nil
            if mod_name then
                -- Track first `mod tests` (or any module containing tests) as container_line candidate.
                if container_line == nil then
                    container_line = node:range()
                end
                table.insert(mod_stack, mod_name)
                for child in node:iter_children() do
                    walk(child, mod_stack)
                end
                table.remove(mod_stack)
                return
            end
        elseif t == "function_item" and fn_has_test_attribute(node) then
            local name_node
            for child in node:iter_children() do
                if child:type() == "identifier" then
                    name_node = child
                    break
                end
            end
            if name_node then
                local fn_name = node_text(name_node)
                local mod_path = table.concat(mod_stack, "::")
                local line = name_node:range()
                table.insert(results, { mod_path = mod_path, fn_name = fn_name, line = line })
            end
        end
        for child in node:iter_children() do
            walk(child, mod_stack)
        end
    end

    walk(root, {})

    -- For file-scope tests (e.g. Rust integration tests) there is no enclosing
    -- `mod` to attach a container summary to. Fall back to the first line so
    -- the user still gets an aggregated pass/fail mark for the file.
    if container_line == nil and #results > 0 then
        container_line = 0
    end

    return results, container_line
end

--- Compute binary_id + within-binary module path from a source file path.
---@param packages table[]
---@param file_path string
---@return string|nil binary_id, string|nil binary_root_mod_path
local function classify_file(packages, file_path)
    file_path = vim.fn.fnamemodify(file_path, ":p")

    -- Pass 1: direct hit on a target's src_path.
    for _, pkg in ipairs(packages) do
        local pkg_name_snake = (pkg.name or ""):gsub("%-", "_")
        for _, target in ipairs(pkg.targets or {}) do
            local src_path = target.src_path and vim.fn.fnamemodify(target.src_path, ":p")
            if src_path == file_path then
                for _, kind in ipairs(target.kind or {}) do
                    local bid = compute_binary_id(pkg_name_snake, kind, target.name)
                    if bid then
                        return bid, ""
                    end
                end
            end
        end
    end

    -- Pass 2: subtree match. Only valid when the target's src_path acts as a
    -- "root" file with a real submodule tree underneath it. Specifically:
    --   * src/lib.rs and src/main.rs  -> subtree is everything in src/ except
    --     bin/<name>.rs files, tests/, examples/, benches/ (those are siblings,
    --     not submodules).
    --   * src/bin/<name>/main.rs      -> subtree is src/bin/<name>/**
    --   * tests/<name>/main.rs        -> subtree is tests/<name>/**
    --   * benches/<name>/main.rs etc. -> same pattern
    -- Bare files like src/bin/<name>.rs or tests/<name>.rs DO NOT have a
    -- submodule subtree; sibling files in the same dir are independent targets.
    for _, pkg in ipairs(packages) do
        local pkg_name_snake = (pkg.name or ""):gsub("%-", "_")
        for _, target in ipairs(pkg.targets or {}) do
            local src_path = target.src_path and vim.fn.fnamemodify(target.src_path, ":p")
            if src_path then
                local basename = vim.fn.fnamemodify(src_path, ":t")
                local src_dir = vim.fn.fnamemodify(src_path, ":h") .. "/"
                local is_root_file = false
                if basename == "lib.rs" or basename == "main.rs" then
                    is_root_file = true
                end
                if is_root_file and vim.startswith(file_path, src_dir) then
                    for _, kind in ipairs(target.kind or {}) do
                        local bid = compute_binary_id(pkg_name_snake, kind, target.name)
                        if bid then
                            local rel = file_path:sub(#src_dir + 1):gsub("%.rs$", "")
                            rel = rel:gsub("/mod$", ""):gsub("/main$", "")
                            -- For src/lib.rs or src/main.rs, exclude sibling
                            -- integration-test / bin / example files.
                            if kind == "lib" or (kind == "bin" and basename == "main.rs") then
                                -- Skip if the relative path enters a sibling target's dir.
                                if
                                    rel:match("^bin/")
                                    or rel:match("^tests/")
                                    or rel:match("^examples/")
                                    or rel:match("^benches/")
                                then
                                    goto continue
                                end
                            end
                            local mod_path = rel:gsub("/", "::")
                            if mod_path == "lib" or mod_path == "main" then
                                mod_path = ""
                            end
                            return bid, mod_path
                        end
                        ::continue::
                    end
                end
            end
        end
    end
    return nil, nil
end

--- Build (or rebuild) the workspace index: container_id -> file_path
--- plus per-file test positions cache.
---@param workspace_root string
local function build_index(workspace_root)
    local cached = index_cache[workspace_root]
    if cached then
        return cached
    end

    local t0 = vim.uv.hrtime()
    local packages = cargo_metadata_packages(workspace_root)
    local container_files = {}
    local positions_by_file = {}

    -- Discover .rs files under each package's manifest dir.
    local seen_files = {}
    local all_files = {}
    for _, pkg in ipairs(packages) do
        local manifest_dir = pkg.manifest_path and vim.fn.fnamemodify(pkg.manifest_path, ":h")
        if manifest_dir then
            for _, target in ipairs(pkg.targets or {}) do
                local src_path = target.src_path and vim.fn.fnamemodify(target.src_path, ":p")
                if src_path and not seen_files[src_path] then
                    seen_files[src_path] = true
                    table.insert(all_files, src_path)
                end
                -- Also walk siblings of the target src_path (sub-modules).
                if src_path then
                    local dir = vim.fn.fnamemodify(src_path, ":h")
                    for _, f in ipairs(vim.fn.glob(dir .. "/**/*.rs", false, true)) do
                        local abs = vim.fn.fnamemodify(f, ":p")
                        if not seen_files[abs] then
                            seen_files[abs] = true
                            table.insert(all_files, abs)
                        end
                    end
                end
            end
        end
    end

    for _, file_path in ipairs(all_files) do
        local binary_id, binary_root_mod = classify_file(packages, file_path)
        if binary_id then
            local tests, container_line = parse_test_fns(file_path, { silent = true })
            if #tests > 0 then
                local fn_positions = {}
                for _, t in ipairs(tests) do
                    -- Full path of the container from binary root:
                    -- binary_root_mod + t.mod_path (concatenated with "::")
                    local parts = {}
                    if binary_root_mod and binary_root_mod ~= "" then
                        table.insert(parts, binary_root_mod)
                    end
                    if t.mod_path and t.mod_path ~= "" then
                        table.insert(parts, t.mod_path)
                    end
                    local mod_path = table.concat(parts, "::")
                    local container_id = mod_path == "" and binary_id or (binary_id .. "::" .. mod_path)
                    container_files[container_id] = file_path
                    fn_positions[t.fn_name] = t.line
                end
                positions_by_file[file_path] = {
                    positions = fn_positions,
                    container_line = container_line,
                }
            end
        end
    end

    log.info(
        string.format(
            "[perf rust_index] %s containers=%d files=%d build=%.1fms",
            workspace_root,
            vim.tbl_count(container_files),
            vim.tbl_count(positions_by_file),
            (vim.uv.hrtime() - t0) / 1e6
        )
    )

    cached = {
        container_files = container_files,
        positions_by_file = positions_by_file,
        built_at = vim.uv.hrtime(),
    }
    index_cache[workspace_root] = cached
    return cached
end

---@param container_id string
---@param report_dir string
---@return string|nil
function M.id_to_file(container_id, report_dir)
    local workspace_root = resolve_workspace_root(report_dir)
    if not workspace_root then
        return nil
    end
    local idx = build_index(workspace_root)
    return idx.container_files[container_id]
end

---@param file_path string
---@param opts? test_report.FindOpts
---@return table<string, number> positions
---@return number|nil container_line
function M.find_test_positions(file_path, opts)
    file_path = vim.fn.fnamemodify(file_path, ":p")

    -- Try the cached index first (built from cargo metadata walk).
    for _, idx in pairs(index_cache) do
        local entry = idx.positions_by_file[file_path]
        if entry then
            return entry.positions, entry.container_line
        end
    end

    -- Fallback: parse on demand.
    local tests, container_line = parse_test_fns(file_path, opts)
    local positions = {}
    for _, t in ipairs(tests) do
        positions[t.fn_name] = t.line
    end
    return positions, container_line
end

---@param container_id string
---@param stacktrace string
---@return number|nil
function M.extract_error_line(container_id, stacktrace)
    if not stacktrace or stacktrace == "" then
        return nil
    end
    local lnum = stacktrace:match("panicked at [^:]+:(%d+):%d+")
    if lnum then
        return tonumber(lnum)
    end
    lnum = stacktrace:match("[%w/_%-%.]+%.rs:(%d+):%d+")
    return lnum and tonumber(lnum) or nil
end

---@return string
function M.get_test_report_dir()
    local ws = cargo_workspace_root_from_cwd(vim.fn.getcwd())
    ws = ws or vim.fn.getcwd()
    return ws .. "/target/nextest/default"
end

---@param id string  Full test id "<binary_id>::<mod_path>#<fn_name>" or "<binary_id>#<fn_name>"
---@return test_report.IdDisplay
function M.id_to_display(id)
    local container_id, member = id:match("^(.+)#(.+)$")
    if not container_id then
        return { container = id, member = "", group = nil }
    end
    -- container_id is "<binary_id>" or "<binary_id>::<mod_path>".
    -- Display: container = last "::" segment; group = everything before.
    local group, container_name = container_id:match("^(.-)::([^:]+)$")
    if not container_name then
        -- No "::" at all - container_id is just <binary_id>, no group.
        return { container = container_id, member = member, group = nil }
    end
    return { container = container_name, member = member, group = group }
end

return M
