-- jest JSON report parser.
--
-- jest emits one JSON file (via `--json --outputFile=`) describing every test
-- file it ran. With `--testLocationInResults` each assertion carries a
-- `location = { line, column }` pointing at the test's source line.
--
-- Shape (trimmed):
--   {
--     "testResults": [
--       {
--         "name": "/abs/path/tests/foo.test.ts",
--         "assertionResults": [
--           { "status": "passed"|"failed"|"pending"|"todo"|"skipped",
--             "title": "leaf title",
--             "fullName": "describe chain leaf title",
--             "ancestorTitles": ["describe", "nested"],
--             "duration": 3,                      // ms
--             "location": { "line": 6, "column": 3 },
--             "failureMessages": ["...ANSI..."] }
--         ]
--       }
--     ]
--   }
--
-- We group a file's assertions by `location.line`. Each line becomes one
-- aggregated test (a `test.each` block expands to many assertions that all
-- share the same source line -> a single test with N invocations).

local file_lib = require("lib.file")
local log = require("utils.logging-util").new({
    name = "test-report-js-json",
    filename = "test-report.log",
    level = vim.log.levels.DEBUG,
})

local M = {}

---@param report_dir string
---@return string[]
function M.list_report_files(report_dir)
    local files = vim.fn.glob(report_dir .. "/*.json", false, true)
    log.debug("list_report_files: " .. report_dir .. " -> " .. #files .. " files")
    return files
end

---@param s string
---@return string
local function clean_ansi(s)
    return (
        s:gsub("\x1b%[%d+;%d+;%d+;%d+;%d+m", "")
            :gsub("\x1b%[%d+;%d+;%d+;%d+m", "")
            :gsub("\x1b%[%d+;%d+;%d+m", "")
            :gsub("\x1b%[%d+;%d+m", "")
            :gsub("\x1b%[%d+m", "")
            :gsub("\x1b%[m", "")
    )
end

--- Find `<file>:<line>:<col>` for the test file inside a failure message.
---@param file string abs path
---@param msg string
---@return number|nil line
local function find_error_line(file, msg)
    local pat = file:gsub("([^%w])", "%%%1") .. "%:(%d+)%:%d+"
    local line = msg:match(pat)
    return line and tonumber(line) or nil
end

---@param status string
---@return "passed"|"failed"|"skipped"
local function norm_status(status)
    if status == "passed" then
        return "passed"
    elseif status == "failed" then
        return "failed"
    end
    -- pending / todo / skipped / disabled
    return "skipped"
end

---@param invocations test_report.Invocation[]
---@return "passed"|"failed"|"skipped"
local function aggregate_status(invocations)
    local any_passed = false
    for _, inv in ipairs(invocations) do
        if inv.status == "failed" then
            return "failed"
        elseif inv.status == "passed" then
            any_passed = true
        end
    end
    return any_passed and "passed" or "skipped"
end

--- A `--testNamePattern` (single-test) run was active for this report dir.
--- In that mode jest emits non-matching tests as `pending`; we must drop them
--- so sibling tests are not clobbered. Written by the runner as `.run-meta`.
---@param dir string
---@return boolean filtered
local function run_was_filtered(dir)
    local content = file_lib.read_file(dir .. "/.run-meta")
    return content ~= nil and content:match("filtered") ~= nil
end

--- Parse one jest JSON report file into per-file, per-line groups.
---@param filepath string
---@return table<string, table<integer, table>> by_file  abs_file -> (row0 -> group)
function M.parse_raw(filepath)
    local content = file_lib.read_file(filepath)
    if not content or content == "" then
        return {}
    end
    local ok, data = pcall(vim.json.decode, content, { luanil = { object = true, array = true } })
    if not ok or type(data) ~= "table" or not data.testResults then
        log.warn("failed to decode jest json: " .. filepath)
        return {}
    end

    local filtered = run_was_filtered(vim.fn.fnamemodify(filepath, ":h"))

    local by_file = {}

    for _, tr in ipairs(data.testResults) do
        local abs = vim.fn.fnamemodify(tr.name, ":p")
        local by_line = by_file[abs] or {}
        by_file[abs] = by_line

        for _, a in ipairs(tr.assertionResults or {}) do
            -- In a filtered (single-test) run, jest reports non-matching tests
            -- as `pending`; skip them so we only update the tests we ran.
            if filtered and a.status == "pending" then
                goto continue_assertion
            end
            local line1 = (a.location and a.location.line) or 1
            local row0 = line1 - 1
            local g = by_line[row0]
            if not g then
                g = {
                    leaf = a.title or "",
                    ancestors = a.ancestorTitles or {},
                    full = a.fullName or a.title or "",
                    invocations = {},
                    errors = {},
                }
                by_line[row0] = g
            end

            local status = norm_status(a.status)
            local stack
            if a.failureMessages and #a.failureMessages > 0 then
                local parts = {}
                for _, fm in ipairs(a.failureMessages) do
                    parts[#parts + 1] = clean_ansi(fm)
                end
                stack = table.concat(parts, "\n\n")
            end

            table.insert(g.invocations, {
                name = a.title or g.leaf,
                status = status,
                stacktrace = stack,
                time = a.duration and (a.duration / 1000) or nil,
            })

            if status == "failed" then
                local eline = (stack and find_error_line(abs, stack)) or line1
                table.insert(g.errors, {
                    message = stack or ((a.title or g.leaf) .. ": failed"),
                    line = eline,
                })
            end

            ::continue_assertion::
        end
    end

    return by_file
end

M.aggregate_status = aggregate_status

return M
