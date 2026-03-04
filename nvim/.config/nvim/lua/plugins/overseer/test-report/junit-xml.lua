local xml = require("lib.xml")
local file = require("lib.file")
local log = require("utils.logging-util").new({ name = "test-report-xml", filename = "test-report.log", level = vim.log.levels.DEBUG })

local M = {}

---@param report_dir string
---@return table<string, test_report.TestResult>
function M.parse_report_dir(report_dir)
    local results = {}
    log.debug("parse_report_dir: " .. report_dir)

    local files = vim.fn.glob(report_dir .. "/TEST-*.xml", false, true)
    log.debug("found " .. #files .. " XML files")
    if #files == 0 then
        vim.notify("No JUnit XML reports found in: " .. report_dir, vim.log.levels.WARN)
        return results
    end

    for _, filepath in ipairs(files) do
        log.debug("parsing: " .. filepath)
        local content = file.read_file(filepath)
        if content then
            log.debug("file content length: " .. #content)
            local ok, parsed = pcall(xml.parse, content)
            if ok and parsed and parsed.testsuite then
                log.debug("XML parsed OK, processing testsuite")
                M._process_testsuite(parsed.testsuite, results)
            elseif not ok then
                log.error("XML parse error: " .. tostring(parsed))
                vim.notify("test-report: failed to parse XML: " .. filepath, vim.log.levels.ERROR)
            else
                log.warn("parsed OK but no testsuite key found")
                vim.notify("test-report: no testsuite in XML: " .. filepath, vim.log.levels.ERROR)
            end
        else
            log.error("could not read file: " .. filepath)
            vim.notify("test-report: could not read file: " .. filepath, vim.log.levels.ERROR)
        end
    end

    log.debug("parse_report_dir done, " .. vim.tbl_count(results) .. " results")
    return results
end

---@param testsuite table
---@param results table<string, test_report.TestResult>
function M._process_testsuite(testsuite, results)
    local testcases = testsuite.testcase
    if not testcases then
        return
    end

    -- Single testcase may not be wrapped in array
    if testcases._attr then
        testcases = { testcases }
    end

    for _, tc in ipairs(testcases) do
        local attr = tc._attr
        if attr then
            local classname = attr.classname or ""
            local name = attr.name or ""
            -- Strip parameterized suffixes like "()" or "[1]"
            local method_name = name:match("^([^%(%)%[%]]+)") or name
            local id = classname .. "#" .. method_name

            -- Capture output streams
            local stdout = M._extract_text(tc["system-out"])
            local stderr = M._extract_text(tc["system-err"])

            if tc.failure then
                local errors, stacktrace = M._extract_errors(classname, tc.failure)
                results[id] = { status = "failed", errors = errors, stdout = stdout, stderr = stderr, stacktrace = stacktrace }
            elseif tc.error then
                local errors, stacktrace = M._extract_errors(classname, tc.error)
                results[id] = { status = "failed", errors = errors, stdout = stdout, stderr = stderr, stacktrace = stacktrace }
            elseif tc.skipped then
                results[id] = { status = "skipped", stdout = stdout, stderr = stderr }
            else
                results[id] = { status = "passed", stdout = stdout, stderr = stderr }
            end
        end
    end
end

--- Extracts text from a parsed XML node.
--- Handles single tag (string after reduce), or multiple tags (array of strings).
---@param node table|string|nil
---@return string|nil
function M._extract_text(node)
    if not node then
        return nil
    end
    if type(node) == "string" then
        return node
    end
    if type(node) == "table" then
        local parts = {}
        for _, v in ipairs(node) do
            if type(v) == "string" then
                parts[#parts + 1] = v
            end
        end
        if #parts > 0 then
            return table.concat(parts)
        end
    end
    return nil
end

---@param classname string
---@param failure_node table|string
---@return { message: string, line: number|nil }[], string
function M._extract_errors(classname, failure_node)
    local errors = {}
    local full_stacktrace = ""

    -- failure_node can be a single table or an array of failures
    local failures = failure_node
    if failures._attr or type(failures) == "string" then
        failures = { failures }
    end

    for _, f in ipairs(failures) do
        local message = ""
        local stacktrace = ""

        if type(f) == "table" then
            if f._attr then
                message = f._attr.message or f._attr.type or ""
            end
            -- The text content of the failure element is the stacktrace
            if f[1] and type(f[1]) == "string" then
                stacktrace = f[1]
            end
        elseif type(f) == "string" then
            stacktrace = f
        end

        if stacktrace ~= "" then
            full_stacktrace = full_stacktrace .. stacktrace
        end

        local line = M._extract_error_line(classname, stacktrace)
        table.insert(errors, { message = message, line = line })
    end

    return errors, full_stacktrace
end

---@param classname string
---@param stacktrace string
---@return number|nil
function M._extract_error_line(classname, stacktrace)
    if not stacktrace or stacktrace == "" then
        return nil
    end
    -- Extract simple class name from fully-qualified (e.g., "com.example.MyTest" -> "MyTest")
    local simple_name = classname:match("([^%.]+)$") or classname
    -- Match "ClassName.java:123" pattern
    local pattern = simple_name .. "%.java:(%d+)"
    local line_str = stacktrace:match(pattern)
    if line_str then
        return tonumber(line_str)
    end
    return nil
end

return M
