-- Go language adapter for test-report.
--
-- Test ID convention (synced with json-parser):
--   <Package>#<TestFn>      e.g. "github.com/me/proj/pkg/util#TestFoo"
--
-- File resolution:
--   * Package import path -> directory via cached `go list -json ./...`.
--   * Test fn name -> file via treesitter scan of every *_test.go in the dir.
--
-- Test discovery (treesitter):
--   * Functions whose name matches ^Test|^Benchmark|^Example
--   * Where the first param's type is one of *testing.T / *testing.B / *testing.F

local json_parser = require("modules.go.test-report.json-parser")
local log = require("utils.logging-util").new({
    name = "test-report-go",
    filename = "test-report.log",
    level = vim.log.levels.DEBUG,
})

---@type test_report.LangAdapter
local M = {
    group_separator = "/",
    diagnostic_source = "go-test",
    trouble_source = "go_test_diagnostics",
}

-- workspace_root -> {
--   pkg_dirs           = { [import_path] = dir },
--   container_files    = { [import_path] = file_path of first test file },
--   fn_to_file         = { [import_path] = { [fn_name] = file_path } },
--   positions_by_file  = { [file_path] = { positions = {...}, container_line = N|nil } },
-- }
local index_cache = {}

function M.clear_cache()
    index_cache = {}
end

---@return string|nil
local function go_module_root_from_cwd(cwd)
    local cmd = string.format("cd %s 2>/dev/null && go env GOMOD 2>/dev/null", vim.fn.shellescape(cwd))
    local h = io.popen(cmd)
    if not h then
        return nil
    end
    local out = h:read("*a") or ""
    h:close()
    local gomod = out:gsub("\n", "")
    if gomod == "" or gomod == "/dev/null" then
        return nil
    end
    return vim.fn.fnamemodify(gomod, ":h")
end

--- Derive module root from a report dir we generate:
---   <cache>/test-report/go/<safe-module-path>/  (no Go path embedded)
--- We can't reverse the safe path, so fall back to cwd.
---@param report_dir string|nil
---@return string|nil
local function workspace_root_from_report_dir(report_dir)
    if not report_dir then
        return nil
    end
    -- Some users may keep reports inside the module (.test-report/go-test.json).
    local m = report_dir:match("^(.+)/%.test%-report/?$")
    if m then
        return m
    end
    return nil
end

---@param report_dir string|nil
---@return string|nil
local function resolve_module_root(report_dir)
    return workspace_root_from_report_dir(report_dir) or go_module_root_from_cwd(vim.fn.getcwd())
end

--- Run `go list -json ./...` in module root, returning package descriptors.
---@param module_root string
---@return table[]
local function go_list_packages(module_root)
    local cmd = string.format("cd %s 2>/dev/null && go list -json ./... 2>/dev/null", vim.fn.shellescape(module_root))
    local h = io.popen(cmd)
    if not h then
        return {}
    end
    local out = h:read("*a") or ""
    h:close()

    -- `go list -json ./...` emits concatenated top-level JSON objects (not a JSON array).
    -- Wrap them into a [array] and let vim.json handle it.
    local packages = {}
    -- Manual brace-depth split.
    local depth = 0
    local start_idx = nil
    for i = 1, #out do
        local ch = out:sub(i, i)
        if ch == "{" then
            if depth == 0 then
                start_idx = i
            end
            depth = depth + 1
        elseif ch == "}" then
            depth = depth - 1
            if depth == 0 and start_idx then
                local chunk = out:sub(start_idx, i)
                local ok, pkg = pcall(vim.fn.json_decode, chunk)
                if ok and type(pkg) == "table" and pkg.ImportPath then
                    table.insert(packages, pkg)
                end
                start_idx = nil
            end
        end
    end
    return packages
end

local _test_query
local function test_query()
    if _test_query then
        return _test_query
    end
    _test_query = vim.treesitter.query.parse(
        "go",
        [[
        (function_declaration
          name: (identifier) @fn.name
          parameters: (parameter_list) @fn.params
        ) @fn.definition
    ]]
    )
    return _test_query
end

--- Check that the function's first param is *testing.T/B/F.
---@param params_node TSNode
---@param bufnr integer
---@return boolean
local function first_param_is_testing(params_node, bufnr)
    for child in params_node:iter_children() do
        if child:type() == "parameter_declaration" then
            local text = vim.treesitter.get_node_text(child, bufnr)
            -- Match "*testing.T" / "*testing.B" / "*testing.F" anywhere in the param text.
            if text:match("%*testing%.[TBF]%f[^%a]") then
                return true
            end
            return false -- only first param matters
        end
    end
    return false
end

--- Match Go's own test/benchmark/fuzz/example naming convention.
--- Per the testing package: a name like `TestXxx` is a test iff `Xxx` does NOT
--- start with a lowercase ASCII letter (i.e. `^(Test|Benchmark|Fuzz|Example)[^a-z]`
--- or the name is exactly the prefix). `TestMain` is the package setup hook and
--- is excluded.
---@param fn_name string
---@return boolean
local function is_test_fn_name(fn_name)
    if fn_name == "TestMain" then
        return false
    end
    for _, prefix in ipairs({ "Test", "Benchmark", "Fuzz", "Example" }) do
        if fn_name == prefix then
            return true
        end
        if vim.startswith(fn_name, prefix) then
            local next_char = fn_name:sub(#prefix + 1, #prefix + 1)
            if next_char ~= "" and not next_char:match("[a-z]") then
                return true
            end
        end
    end
    return false
end

--- Parse a Go test file and yield test fns plus the container line (always 0 for Go
--- since tests live at file scope; container summary lands on line 1).
---@param file_path string
---@param opts? test_report.FindOpts
---@return { fn_name: string, line: integer }[] tests
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

    local ok, parser = pcall(vim.treesitter.get_parser, bufnr, "go")
    if not ok or not parser then
        log.error("go treesitter parser failed: " .. tostring(parser))
        return {}, nil
    end
    local tree = parser:parse()[1]
    if not tree then
        return {}, nil
    end
    local root = tree:root()

    local results = {}
    local q = test_query()
    -- Map capture name -> id
    local capture_ids = {}
    for i, name in ipairs(q.captures) do
        capture_ids[name] = i
    end
    for _, match in q:iter_matches(root, bufnr, 0, -1, { all = true }) do
        local name_nodes = match[capture_ids["fn.name"]]
        local params_nodes = match[capture_ids["fn.params"]]
        local name_node = name_nodes and name_nodes[1]
        local params_node = params_nodes and params_nodes[1]
        if name_node and params_node then
            local fn_name = vim.treesitter.get_node_text(name_node, bufnr)
            if is_test_fn_name(fn_name) then
                local is_example = fn_name == "Example" or fn_name:match("^Example[%u_]")
                if is_example or first_param_is_testing(params_node, bufnr) then
                    local line = name_node:range()
                    table.insert(results, { fn_name = fn_name, line = line })
                end
            end
        end
    end

    -- For Go we put the container summary on line 0 (no enclosing class/mod).
    local container_line = (#results > 0) and 0 or nil
    return results, container_line
end

--- Build (or rebuild) the workspace index.
---@param module_root string
local function build_index(module_root)
    local cached = index_cache[module_root]
    if cached then
        return cached
    end

    local t0 = vim.uv.hrtime()
    local packages = go_list_packages(module_root)
    local pkg_dirs = {}
    local container_files = {}
    local fn_to_file = {}
    local positions_by_file = {}

    for _, pkg in ipairs(packages) do
        local import_path = pkg.ImportPath
        local dir = pkg.Dir
        if import_path and dir then
            pkg_dirs[import_path] = dir
            -- Collect *_test.go files from this package (TestGoFiles + XTestGoFiles).
            local test_files = {}
            for _, f in ipairs(pkg.TestGoFiles or {}) do
                table.insert(test_files, dir .. "/" .. f)
            end
            for _, f in ipairs(pkg.XTestGoFiles or {}) do
                table.insert(test_files, dir .. "/" .. f)
            end
            local first_file
            for _, f in ipairs(test_files) do
                local abs = vim.fn.fnamemodify(f, ":p")
                local tests, container_line = parse_test_fns(abs, { silent = true })
                if #tests > 0 then
                    if not first_file then
                        first_file = abs
                    end
                    local fn_positions = {}
                    fn_to_file[import_path] = fn_to_file[import_path] or {}
                    for _, t in ipairs(tests) do
                        fn_positions[t.fn_name] = t.line
                        fn_to_file[import_path][t.fn_name] = abs
                    end
                    positions_by_file[abs] = {
                        positions = fn_positions,
                        container_line = container_line,
                    }
                end
            end
            if first_file then
                container_files[import_path] = first_file
            end
        end
    end

    log.info(
        string.format(
            "[perf go_index] %s pkgs=%d files=%d build=%.1fms",
            module_root,
            vim.tbl_count(pkg_dirs),
            vim.tbl_count(positions_by_file),
            (vim.uv.hrtime() - t0) / 1e6
        )
    )

    cached = {
        pkg_dirs = pkg_dirs,
        container_files = container_files,
        fn_to_file = fn_to_file,
        positions_by_file = positions_by_file,
        built_at = vim.uv.hrtime(),
    }
    index_cache[module_root] = cached
    return cached
end

---@param dirs string[]
---@return table<string, test_report.TestResult>
function M.parse_results(dirs)
    local raw = json_parser.parse_results(dirs)

    -- Per-test file-level resolution: when a test fn lives in a specific file
    -- (not the index's "first test file"), reroute id_to_file by tweaking the
    -- container_files index lazily. We don't need to rewrite results here.
    return raw
end

---@param container_id string  Go package import path
---@param report_dir string
---@return string|nil
function M.id_to_file(container_id, report_dir)
    local module_root = resolve_module_root(report_dir)
    if not module_root then
        return nil
    end
    local idx = build_index(module_root)
    return idx.container_files[container_id]
end

---@param file_path string
---@param opts? test_report.FindOpts
---@return table<string, number> positions
---@return number|nil container_line
function M.find_test_positions(file_path, opts)
    file_path = vim.fn.fnamemodify(file_path, ":p")
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
    local lnum = stacktrace:match("[%w/_%-%.]+%.go:(%d+):%s")
    if lnum then
        return tonumber(lnum)
    end
    lnum = stacktrace:match("[%w/_%-%.]+%.go:(%d+):%d+")
    return lnum and tonumber(lnum) or nil
end

---@return string
function M.get_test_report_dir()
    local module_root = go_module_root_from_cwd(vim.fn.getcwd()) or vim.fn.getcwd()
    local safe = module_root:gsub("[/\\]", "_"):gsub("^_+", "")
    return vim.fn.stdpath("cache") .. "/test-report/go/" .. safe
end

---@param id string  "<package>#<TestFn>"
---@return test_report.IdDisplay
function M.id_to_display(id)
    local container_id, member = id:match("^(.+)#(.+)$")
    if not container_id then
        return { container = id, member = "", group = nil }
    end
    -- container_id is the full Go package import path (slash-separated).
    local group, container_name = container_id:match("^(.-)/([^/]+)$")
    if not container_name then
        return { container = container_id, member = member, group = nil }
    end
    return { container = container_name, member = member, group = group }
end

return M
