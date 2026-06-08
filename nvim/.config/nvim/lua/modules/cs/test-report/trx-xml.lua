-- Parses `dotnet test` TRX (Visual Studio TeamTest) XML reports into TestResult
-- structures consumed by modules.common.test-report.
--
-- TRX shape (namespace-stripped by lib.xml):
--   TestRun.TestDefinitions.UnitTest[]   -> { _attr.id, TestMethod._attr.className/name }
--   TestRun.Results.UnitTestResult[]     -> { _attr.testId/testName/outcome/duration,
--                                             Output.ErrorInfo.Message/StackTrace }
--
-- We join result.testId -> definition.id to get className + method, then build a
-- stable test id `<className>#<method>`. Parametrized cases (several results sharing
-- the same className.method) are aggregated into multiple invocations of one test.

local xml = require("lib.xml")
local file = require("lib.file")
local log = require("utils.logging-util").new({
    name = "test-report-cs-trx",
    filename = "test-report.log",
    level = vim.log.levels.DEBUG,
})

local M = {}

---@param report_dir string
---@return string[]
function M.list_report_files(report_dir)
    local files = vim.fn.glob(report_dir .. "/*.trx", false, true)
    log.debug("list_report_files: " .. report_dir .. " -> " .. #files)
    return files
end

---@param outcome string|nil
---@return "passed"|"failed"|"skipped"
local function map_outcome(outcome)
    if outcome == "Passed" then
        return "passed"
    elseif outcome == "Failed" then
        return "failed"
    end
    return "skipped"
end

---@param a string
---@param b string
---@return string
local function pick_status(a, b)
    if a == "failed" or b == "failed" then
        return "failed"
    end
    if a == "passed" or b == "passed" then
        return "passed"
    end
    return a or b or "skipped"
end

--- "00:00:00.0048150" -> seconds (number)
---@param dur string|nil
---@return number|nil
local function parse_duration(dur)
    if not dur then
        return nil
    end
    local h, m, s = dur:match("^(%d+):(%d+):([%d%.]+)$")
    if not h then
        return nil
    end
    return tonumber(h) * 3600 + tonumber(m) * 60 + tonumber(s)
end

---@param node table|nil
---@return string|nil
local function text_of(node)
    if type(node) == "string" then
        return node
    end
    if type(node) == "table" and node[1] and type(node[1]) == "string" then
        return node[1]
    end
    return nil
end

---@param line_num_from_stack string
---@return number|nil
local function line_from_stacktrace(stacktrace)
    if not stacktrace or stacktrace == "" then
        return nil
    end
    local lnum = stacktrace:match(":line (%d+)")
    return lnum and tonumber(lnum) or nil
end

--- Normalise a TRX list child (single element vs array) into an array.
---@param node any
---@return table[]
local function as_array(node)
    if node == nil then
        return {}
    end
    if node._attr then
        return { node }
    end
    return node
end

--- Build testId -> { className, method } from TestDefinitions.
---@param test_run table
---@return table<string, { className: string, method: string }>
local function build_definitions(test_run)
    local defs = {}
    local td = test_run.TestDefinitions
    if not td then
        return defs
    end
    for _, ut in ipairs(as_array(td.UnitTest)) do
        local id = ut._attr and ut._attr.id
        local tm = ut.TestMethod
        if id and tm and tm._attr then
            defs[id] = {
                className = tm._attr.className or "",
                method = tm._attr.name or "",
            }
        end
    end
    return defs
end

--- Strip the "<className>." prefix from a TRX testName to get a readable case label.
---@param test_name string
---@param class_name string
---@return string
local function case_label(test_name, class_name)
    if not test_name or test_name == "" then
        return ""
    end
    local prefix = class_name .. "."
    if class_name ~= "" and test_name:sub(1, #prefix) == prefix then
        return test_name:sub(#prefix + 1)
    end
    return test_name
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
    -- Strip UTF-8 BOM if present (TRX files are written with one).
    if content:byte(1) == 239 and content:byte(2) == 187 and content:byte(3) == 191 then
        content = content:sub(4)
    end
    local ok, parsed = pcall(xml.parse, content)
    if not ok or not parsed then
        log.error("XML parse error: " .. tostring(parsed))
        vim.notify("test-report: failed to parse TRX: " .. filepath, vim.log.levels.ERROR)
        return results
    end

    local test_run = parsed.TestRun or parsed
    local defs = build_definitions(test_run)
    local res_root = test_run.Results
    if not res_root then
        log.warn("no Results in: " .. filepath)
        return results
    end

    for _, utr in ipairs(as_array(res_root.UnitTestResult)) do
        local attr = utr._attr
        if attr and attr.testId then
            local def = defs[attr.testId] or {}
            local class_name = def.className or ""
            -- NUnit encodes the case args in TestMethod.name (e.g. "TestCase_IsEven(100)").
            -- Strip a trailing "(...)" so the member matches the C# method name and all
            -- parametrized cases aggregate under one test (xunit/mstest already do this).
            local method = (def.method or ""):gsub("%b()%s*$", "")
            if class_name ~= "" and method ~= "" then
                local id = class_name .. "#" .. method
                local status = map_outcome(attr.outcome)
                local time = parse_duration(attr.duration)

                local message, stacktrace
                if utr.Output and utr.Output.ErrorInfo then
                    message = text_of(utr.Output.ErrorInfo.Message)
                    stacktrace = text_of(utr.Output.ErrorInfo.StackTrace)
                end

                local invocation = {
                    name = case_label(attr.testName or method, class_name),
                    status = status,
                    metadata = text_of(utr.Output and utr.Output.StdOut),
                    stdout = nil,
                    stderr = nil,
                    stacktrace = stacktrace,
                    time = time,
                }

                local existing = results[id]
                if not existing then
                    local errors
                    if status == "failed" then
                        errors = { { message = message or "Test failed", line = line_from_stacktrace(stacktrace) } }
                    end
                    results[id] = {
                        status = status,
                        errors = errors,
                        time = time,
                        invocations = { invocation },
                    }
                else
                    existing.status = pick_status(existing.status, status)
                    existing.time = (existing.time or 0) + (time or 0)
                    table.insert(existing.invocations, invocation)
                    if status == "failed" then
                        existing.errors = existing.errors or {}
                        table.insert(
                            existing.errors,
                            { message = message or "Test failed", line = line_from_stacktrace(stacktrace) }
                        )
                    end
                end
            end
        end
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
