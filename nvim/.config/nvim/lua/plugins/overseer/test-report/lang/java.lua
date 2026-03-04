local java_util = require("utils.java.java-common")
local log = require("utils.logging-util").new({ name = "test-report-java", filename = "test-report.log", level = vim.log.levels.DEBUG })

---@type test_report.LangAdapter
local M = {}

---@param classname string Fully-qualified Java class name (e.g., "com.example.MyTest")
---@param report_dir string Path to report directory (used to derive project root)
---@return string|nil
function M.classname_to_file(classname, report_dir)
    -- Handle inner classes: take the outer class
    local outer_class = classname:match("^([^%$]+)") or classname
    local relative_path = outer_class:gsub("%.", "/") .. ".java"
    log.debug("classname_to_file: classname=" .. classname .. " relative_path=" .. relative_path)

    -- Derive project root from report_dir (/project/target/junit-report -> /project)
    local project_root = report_dir:match("(.+)/target/junit%-report$")
    log.debug("project_root=" .. tostring(project_root))

    if project_root then
        local glob_pattern = project_root .. "/**/" .. relative_path
        log.debug("glob pattern: " .. glob_pattern)
        local file_path = vim.fn.glob(glob_pattern)
        log.debug("glob result: '" .. tostring(file_path) .. "'")
        if file_path and #file_path ~= 0 then
            local result = vim.split(file_path, "\n")[1]
            log.debug("resolved to: " .. result)
            return result
        end
    end

    -- Fallback: try CWD-relative (original behavior)
    log.debug("falling back to CWD-relative lookup")
    return java_util.java_class_to_proj_path(outer_class)
end

---@param file_path string
---@return table<string, number> method_name -> 0-indexed line number
function M.find_test_positions(file_path)
    local positions = {}
    log.debug("find_test_positions: " .. file_path)

    local bufnr = vim.fn.bufnr(file_path)
    local buf_loaded = bufnr ~= -1
    if not buf_loaded then
        log.debug("buffer not loaded, adding: " .. file_path)
        bufnr = vim.fn.bufadd(file_path)
        vim.fn.bufload(bufnr)
    end
    log.debug("bufnr=" .. bufnr)

    local ok, parser = pcall(vim.treesitter.get_parser, bufnr, "java")
    if not ok or not parser then
        log.warn("treesitter parser failed for bufnr=" .. bufnr .. " err=" .. tostring(parser))
        return positions
    end

    local query_str = [[
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

    local query = vim.treesitter.query.parse("java", query_str)
    local tree = parser:parse()[1]
    if not tree then
        return positions
    end

    for id, node, _ in query:iter_captures(tree:root(), bufnr) do
        local name = query.captures[id]
        if name == "test.name" then
            local method_name = vim.treesitter.get_node_text(node, bufnr)
            local row, _, _, _ = node:range()
            positions[method_name] = row -- 0-indexed
        end
    end

    return positions
end

---@param classname string
---@param stacktrace string
---@return number|nil
function M.extract_error_line(classname, stacktrace)
    if not stacktrace or stacktrace == "" then
        return nil
    end
    local simple_name = classname:match("([^%.]+)$") or classname
    local pattern = simple_name .. "%.java:(%d+)"
    local line_str = stacktrace:match(pattern)
    if line_str then
        return tonumber(line_str)
    end
    return nil
end

---@return string
function M.get_test_report_dir()
    return java_util.get_buffer_project_path() .. "/target/junit-report"
end

return M
