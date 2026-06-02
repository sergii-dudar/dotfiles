-- Parses `go test -json` NDJSON output into TestResult structures.
-- Output shape mirrors what modules.common.test-report expects.
--
-- Each line in the report file is a JSON event:
--   { "Time": "...", "Action": "...", "Package": "...", "Test": "...",
--     "Output": "...", "Elapsed": 0.123 }
--
-- Actions of interest:
--   * run    — test started
--   * pass / fail / skip — terminal status for that Test (or Package, if Test absent)
--   * output — captured stdout/stderr line for that Test (or Package)
--   * pause / cont — concurrency markers (ignored)
--   * start — package compilation start (Test absent; ignored for parsing)
--
-- Test ID convention (synced with go.lua adapter):
--   <Package>#<TestName>
--
-- Subtests:
--   Go reports `TestParent/sub_name` as a separate event stream. We aggregate
--   each `Parent/sub` event under its parent test as an additional invocation
--   (matches the parametrized-aggregation behavior used for Rust).

local file = require("lib.file")
local log = require("utils.logging-util").new({
    name = "test-report-go-json",
    filename = "test-report.log",
    level = vim.log.levels.DEBUG,
})

local M = {}

---@param report_dir string
---@return string[]
function M.list_report_files(report_dir)
    log.debug("list_report_files: " .. report_dir)
    local files = vim.fn.glob(report_dir .. "/*.json", false, true)
    log.debug("found " .. #files .. " JSON files")
    return files
end

---@param output string
---@return number|nil
local function extract_line_from_output(output)
    if not output or output == "" then
        return nil
    end
    -- Typical assertion failure formats:
    --   "    file_test.go:42: ..."
    --   "<file>.go:LINE:COL: ..."
    local lnum = output:match("[%w/_%-%.]+%.go:(%d+):%s")
    if lnum then
        return tonumber(lnum)
    end
    lnum = output:match("[%w/_%-%.]+%.go:(%d+):%d+")
    return lnum and tonumber(lnum) or nil
end

---@param output string
---@return string|nil
local function first_meaningful_line(output)
    if not output then
        return nil
    end
    for line in output:gmatch("[^\n]+") do
        local trimmed = line:gsub("^%s+", "")
        if trimmed ~= "" and not trimmed:match("^=== RUN") and not trimmed:match("^=== PAUSE") then
            return trimmed
        end
    end
    return nil
end

--- Split a Go test name into parent + subtest suffix.
--- Returns parent name and the trailing "/sub/..." piece or nil if not a subtest.
---@param test_name string
---@return string parent, string|nil sub_suffix
local function split_subtest(test_name)
    local parent, sub = test_name:match("^([^/]+)/(.+)$")
    if parent then
        return parent, sub
    end
    return test_name, nil
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

local action_to_status = {
    pass = "passed",
    fail = "failed",
    skip = "skipped",
}

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

    -- Per-(package,test) accumulator before we collapse into parent tests.
    -- key = "<Package>::<Test>"  (NOT the final test_report id)
    local agg = {}
    local function get(pkg, test)
        local key = pkg .. "::" .. test
        local entry = agg[key]
        if not entry then
            entry = {
                package = pkg,
                test = test,
                output_buf = {},
                status = nil,
                elapsed = nil,
            }
            agg[key] = entry
        end
        return entry
    end

    local lines = vim.split(content, "\n", { plain = true })
    for _, line in ipairs(lines) do
        if line ~= "" then
            local ok, ev = pcall(vim.fn.json_decode, line)
            if ok and type(ev) == "table" and ev.Action and ev.Package and ev.Test and ev.Test ~= "" then
                local action = ev.Action
                local entry = get(ev.Package, ev.Test)
                if action == "output" and ev.Output then
                    table.insert(entry.output_buf, ev.Output)
                elseif action_to_status[action] then
                    entry.status = action_to_status[action]
                    entry.elapsed = tonumber(ev.Elapsed) or entry.elapsed
                end
            end
        end
    end

    -- Collapse subtests into parent invocations.
    -- Build final results keyed by `<Package>#<ParentTest>`.
    for _, entry in pairs(agg) do
        local parent, sub = split_subtest(entry.test)
        local id = entry.package .. "#" .. parent
        local status = entry.status or "skipped"
        local output = table.concat(entry.output_buf, "")
        local err_line = (status == "failed") and extract_line_from_output(output) or nil
        local message = (status == "failed") and (first_meaningful_line(output) or "test failed") or nil

        local invocation = {
            name = sub or parent,
            status = status,
            stdout = output ~= "" and output or nil,
            stderr = nil,
            stacktrace = (status == "failed" and output ~= "") and output or nil,
            time = entry.elapsed,
        }

        local existing = results[id]
        if not existing then
            results[id] = {
                status = status,
                errors = message and { { message = message, line = err_line } } or nil,
                time = entry.elapsed,
                invocations = { invocation },
            }
        else
            existing.status = pick_status(existing.status, status)
            existing.time = (existing.time or 0) + (entry.elapsed or 0)
            if message then
                existing.errors = existing.errors or {}
                table.insert(existing.errors, { message = message, line = err_line })
            end
            table.insert(existing.invocations, invocation)
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
