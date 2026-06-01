-- Parses cargo-nextest JUnit XML reports into TestResult structures.
-- Output structure matches what modules.common.test-report expects.
--
-- Nextest XML format (per binary):
--   <testsuite name="<binary_id>">
--     <testcase classname="<binary_id>" name="<module_path::fn_name>" time="...">
--       <failure message="..." type="...">stacktrace</failure>?
--       <system-out>...</system-out>?
--       <system-err>...</system-err>?
--     </testcase>
--   </testsuite>
--
-- We synthesize a stable test ID as: `<binary_id>::<module_path>#<fn_name>`
-- (or `<binary_id>#<fn_name>` if test is at root of the binary).

local xml = require("lib.xml")
local file = require("lib.file")
local log = require("utils.logging-util").new({
    name = "test-report-rust-xml",
    filename = "test-report.log",
    level = vim.log.levels.DEBUG,
})

local M = {}

---@param report_dir string
---@return string[]
function M.list_report_files(report_dir)
    log.debug("list_report_files: " .. report_dir)
    local files = vim.fn.glob(report_dir .. "/*.xml", false, true)
    log.debug("found " .. #files .. " XML files")
    return files
end

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

---@param node table|string|nil
---@return string|nil
local function extract_text(node)
    if not node then
        return nil
    end
    local parts = {}
    collect_text(node, parts)
    return #parts > 0 and table.concat(parts) or nil
end

---@param stacktrace string
---@return string|nil
local function message_from_stacktrace(stacktrace)
    if not stacktrace or stacktrace == "" then
        return nil
    end
    -- First non-empty line of the panic, typically "panicked at src/foo.rs:LINE:COL"
    for line in stacktrace:gmatch("[^\n]+") do
        if line:match("%S") then
            return line
        end
    end
    return nil
end

---@param stacktrace string
---@return number|nil
local function extract_line_from_stacktrace(stacktrace)
    if not stacktrace or stacktrace == "" then
        return nil
    end
    -- Match "panicked at <path>.rs:LINE:COL" or "<path>.rs:LINE:COL"
    local lnum = stacktrace:match("panicked at [^:]+:(%d+):%d+")
    if lnum then
        return tonumber(lnum)
    end
    lnum = stacktrace:match("[%w/_%-%.]+%.rs:(%d+):%d+")
    return lnum and tonumber(lnum) or nil
end

---@param failure_node table|string
---@return { message: string, line: number|nil }[], string
local function extract_errors(failure_node)
    local errors = {}
    local full_stacktrace = ""

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
            if f[1] and type(f[1]) == "string" then
                stacktrace = f[1]
            end
            if f[2] and type(f[2]) == "string" then
                stacktrace = stacktrace .. f[2]
            end
        elseif type(f) == "string" then
            stacktrace = f
        end

        if message == "" then
            message = message_from_stacktrace(stacktrace) or ""
        end

        if stacktrace ~= "" then
            full_stacktrace = full_stacktrace .. stacktrace
        end

        local line = extract_line_from_stacktrace(stacktrace)
        table.insert(errors, { message = message, line = line })
    end

    return errors, full_stacktrace
end

--- Synthesize internal test ID from binary_id + nextest name.
--- name = "module::path::fn_name" or just "fn_name" if at root of the binary.
---@param binary_id string
---@param name string
---@return string id, string container_id, string member
local function build_id(binary_id, name)
    local module_path, fn_name = name:match("^(.+)::([^:]+)$")
    if not module_path then
        -- Test at the root of the binary
        return binary_id .. "#" .. name, binary_id, name
    end
    local container_id = binary_id .. "::" .. module_path
    return container_id .. "#" .. fn_name, container_id, fn_name
end

---@param testsuite table
---@param results table<string, test_report.TestResult>
local function process_testsuite(testsuite, results)
    local suite_attr = testsuite._attr or {}
    local binary_id = suite_attr.name or "unknown"

    local testcases = testsuite.testcase
    if not testcases then
        return
    end
    if testcases._attr then
        testcases = { testcases }
    end

    for _, tc in ipairs(testcases) do
        local attr = tc._attr
        if attr then
            local name = attr.name or ""
            local id, _, fn_name = build_id(binary_id, name)
            local time = tonumber(attr.time)

            local metadata = extract_text(tc["system-out"])
            local stderr = extract_text(tc["system-err"])

            local status, errors, stacktrace
            if tc.failure then
                errors, stacktrace = extract_errors(tc.failure)
                status = "failed"
            elseif tc.error then
                errors, stacktrace = extract_errors(tc.error)
                status = "failed"
            elseif tc.skipped then
                status = "skipped"
            else
                status = "passed"
            end

            local invocation = {
                name = fn_name,
                status = status,
                metadata = metadata,
                stdout = nil,
                stderr = stderr,
                stacktrace = stacktrace,
                time = time,
            }

            results[id] = {
                status = status,
                errors = errors,
                time = time,
                invocations = { invocation },
            }
        end
    end
end

---@param filepath string
---@return table<string, test_report.TestResult>
function M.parse_file(filepath)
    local results = {}
    log.debug("parsing: " .. filepath)
    local content = file.read_file(filepath)
    if not content then
        vim.notify("test-report: could not read file: " .. filepath, vim.log.levels.ERROR)
        return results
    end
    local ok, parsed = pcall(xml.parse, content)
    if not ok or not parsed then
        log.error("XML parse error: " .. tostring(parsed))
        vim.notify("test-report: failed to parse XML: " .. filepath, vim.log.levels.ERROR)
        return results
    end

    -- Nextest wraps everything in <testsuites>...<testsuite>...</testsuite></testsuites>.
    local root = parsed.testsuites or parsed
    local suites = root.testsuite
    if not suites then
        log.warn("no testsuite key in: " .. filepath)
        return results
    end
    if suites._attr then
        suites = { suites }
    end
    for _, suite in ipairs(suites) do
        process_testsuite(suite, results)
    end

    log.debug("parse_file done, " .. vim.tbl_count(results) .. " results")
    return results
end

---@param dirs string[]
---@return table<string, test_report.TestResult>
function M.parse_results(dirs)
    local results = {}
    for _, dir in ipairs(dirs) do
        for _, filepath in ipairs(M.list_report_files(dir)) do
            for id, r in pairs(M.parse_file(filepath)) do
                results[id] = r
            end
        end
    end
    return results
end

return M
