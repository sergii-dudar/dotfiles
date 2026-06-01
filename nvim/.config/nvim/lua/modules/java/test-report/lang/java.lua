local java_util = require("utils.java.java-common")
local junit_xml = require("modules.java.test-report.junit-xml")
local log = require("utils.logging-util").new({
    name = "test-report-java",
    filename = "test-report.log",
    level = vim.log.levels.DEBUG,
})

---@type test_report.LangAdapter
local M = {
    group_separator = ".",
    diagnostic_source = "junit",
    trouble_source = "junit_diagnostics",
}

---@param dirs string[]
---@return table<string, test_report.TestResult>
function M.parse_results(dirs)
    local results = {}
    for _, dir in ipairs(dirs) do
        for _, filepath in ipairs(junit_xml.list_report_files(dir)) do
            for id, r in pairs(junit_xml.parse_file(filepath)) do
                results[id] = r
            end
        end
    end
    return results
end

---@param id string  Full test id "pkg.Class#method"
---@return test_report.IdDisplay
function M.id_to_display(id)
    local container_id, member = id:match("^(.+)#(.+)$")
    if not container_id then
        return { container = id, member = "", group = nil }
    end
    local group = container_id:match("^(.+)%.[^%.]+$")
    local container_name = container_id:match("([^%.]+)$") or container_id
    return { container = container_name, member = member, group = group }
end

-- project_root -> { "com/foo/Bar.java" -> "/abs/path/to/com/foo/Bar.java" }
local class_index_cache = {}

local _test_query
local function test_query()
    if _test_query then
        return _test_query
    end
    _test_query = vim.treesitter.query.parse(
        "java",
        [[
        (class_declaration
          name: (identifier) @class.name
        ) @class.definition

        (method_declaration
          (modifiers
            [
              (marker_annotation
                name: (identifier) @annotation
                (#any-of? @annotation "Test" "ParameterizedTest" "TestFactory" "CartesianTest")
              )
              (annotation
                name: (identifier) @annotation
                (#any-of? @annotation "Test" "ParameterizedTest" "TestFactory" "CartesianTest")
              )
            ]
          )
          name: (identifier) @test.name
        ) @test.definition
    ]]
    )
    return _test_query
end

--- One filesystem walk per project root, indexed by Java source-layout suffix
--- ("com/foo/Bar.java"). Replaces N recursive globs with N hash lookups.
--- Covers Maven/Gradle conventions (src/main/java, src/test/java, src/integTest/java)
--- including multi-module layouts since the prefix can match anywhere in the path.
---@param project_root string
---@return table<string, string>
local function build_class_index(project_root)
    local cached = class_index_cache[project_root]
    if cached then
        return cached
    end
    local t0 = vim.uv.hrtime()
    local index = {}
    for _, abs_path in ipairs(vim.fn.glob(project_root .. "/**/*.java", false, true)) do
        local suffix = abs_path:match("/src/[^/]+/java/(.+)$")
        if suffix then
            index[suffix] = abs_path
        end
    end
    class_index_cache[project_root] = index
    log.info(
        string.format(
            "[perf class_index] %s entries=%d build=%.1fms",
            project_root,
            vim.tbl_count(index),
            (vim.uv.hrtime() - t0) / 1e6
        )
    )
    return index
end

function M.clear_cache()
    class_index_cache = {}
    _test_query = nil
end

--- Resolve a container_id (fully-qualified Java class name) to a source file path.
---@param classname string Fully-qualified Java class name (e.g., "com.example.MyTest")
---@param report_dir string Path to report directory (used to derive project root)
---@return string|nil
function M.id_to_file(classname, report_dir)
    -- Inner classes (Outer$Inner) live in the outer class file
    local outer_class = classname:match("^([^%$]+)") or classname
    local relative_path = outer_class:gsub("%.", "/") .. ".java"

    local project_root = report_dir:match("(.+)/target/junit%-report$")
    if project_root then
        local hit = build_class_index(project_root)[relative_path]
        if hit then
            return hit
        end
    end
    return java_util.java_class_to_proj_path(outer_class)
end

---@param file_path string
---@param opts? test_report.FindOpts
---@return table<string, number> method_name -> 0-indexed line number
---@return number|nil class_line 0-indexed line of class declaration
function M.find_test_positions(file_path, opts)
    local positions = {}
    local class_line
    local silent = not opts or opts.silent ~= false

    local bufnr = vim.fn.bufadd(file_path)
    if not vim.api.nvim_buf_is_loaded(bufnr) then
        if silent then
            -- Load content without firing FileType/BufRead autocmds — skips the
            -- JDTLS attach + highlight cascade. Buffer has no filetype until the
            -- rescue autocmd (or anything else) triggers detection.
            pcall(vim.cmd, "noautocmd call bufload(" .. bufnr .. ")")
        else
            vim.fn.bufload(bufnr)
        end
    end

    local ok, parser = pcall(vim.treesitter.get_parser, bufnr, "java")
    if not ok or not parser then
        log.error("treesitter parser failed for bufnr=" .. bufnr .. " err=" .. tostring(parser))
        vim.notify("test-report: treesitter parser failed for " .. file_path, vim.log.levels.ERROR)
        return positions, class_line
    end

    local tree = parser:parse()[1]
    if not tree then
        return positions, class_line
    end

    local query = test_query()
    for id, node in query:iter_captures(tree:root(), bufnr) do
        local capture = query.captures[id]
        if capture == "class.name" then
            class_line = node:range()
        elseif capture == "test.name" then
            positions[vim.treesitter.get_node_text(node, bufnr)] = node:range()
        end
    end

    return positions, class_line
end

---@param classname string
---@param stacktrace string
---@return number|nil
function M.extract_error_line(classname, stacktrace)
    if not stacktrace or stacktrace == "" then
        return nil
    end
    local simple_name = classname:match("([^%.]+)$") or classname
    local line_str = stacktrace:match(simple_name .. "%.java:(%d+)")
    return line_str and tonumber(line_str) or nil
end

---@return string
function M.get_test_report_dir()
    return java_util.get_buffer_project_path() .. require("utils.constants").java.junit_report_dir
end

return M
