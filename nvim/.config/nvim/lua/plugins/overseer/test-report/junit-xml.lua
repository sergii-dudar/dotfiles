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

            -- Capture output streams (separate JUnit metadata from actual stdout)
            local metadata, stdout = M._extract_system_out(tc["system-out"])
            local stderr = M._extract_text(tc["system-err"])
            local time = tonumber(attr.time)

            local status, errors, stacktrace
            if tc.failure then
                errors, stacktrace = M._extract_errors(classname, tc.failure)
                status = "failed"
            elseif tc.error then
                errors, stacktrace = M._extract_errors(classname, tc.error)
                status = "failed"
            elseif tc.skipped then
                status = "skipped"
            else
                status = "passed"
            end

            local invocation = {
                name = name,
                status = status,
                metadata = metadata,
                stdout = stdout,
                stderr = stderr,
                stacktrace = stacktrace,
                time = time,
            }

            -- Merge parameterized invocations into a single result
            local existing = results[id]
            if existing then
                if status == "failed" then
                    existing.status = "failed"
                end
                if errors then
                    existing.errors = existing.errors or {}
                    vim.list_extend(existing.errors, errors)
                end
                existing.time = (existing.time or 0) + (time or 0)
                table.insert(existing.invocations, invocation)
            else
                results[id] = {
                    status = status,
                    errors = errors,
                    time = time,
                    invocations = { invocation },
                }
            end
        end
    end
end

---@param a string|nil
---@param b string|nil
---@return string|nil
function M._concat_text(a, b)
    if a and b then
        return a .. b
    end
    return a or b
end

--- Recursively collects all string fragments from a parsed XML node.
---@param node table|string|nil
---@param parts string[]
local function collect_text(node, parts)
    if type(node) == "string" then
        parts[#parts + 1] = node
    elseif type(node) == "table" then
        for _, v in ipairs(node) do
            collect_text(v, parts)
        end
    end
end

--- Splits system-out into JUnit metadata (first element) and actual stdout (rest).
--- The first <system-out> CDATA always contains unique-id/display-name metadata.
---@param node table|string|nil
---@return string|nil metadata, string|nil stdout
function M._extract_system_out(node)
    if not node then
        return nil, nil
    end
    -- Single <system-out> tag (reduced to string): metadata only, no stdout
    if type(node) == "string" then
        return node, nil
    end
    -- Multiple <system-out> tags: first is metadata, rest is stdout
    if type(node) == "table" and #node > 0 then
        local metadata = M._extract_text(node[1])
        local parts = {}
        for i = 2, #node do
            collect_text(node[i], parts)
        end
        local stdout = #parts > 0 and table.concat(parts) or nil
        return metadata, stdout
    end
    return nil, nil
end

--- Extracts text from a parsed XML node.
--- Handles single tag (string after reduce), multiple tags (array of strings),
--- and nested tables from split CDATA sections (e.g. escaped ]]> in content).
---@param node table|string|nil
---@return string|nil
function M._extract_text(node)
    if not node then
        return nil
    end
    local parts = {}
    collect_text(node, parts)
    if #parts > 0 then
        return table.concat(parts)
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
