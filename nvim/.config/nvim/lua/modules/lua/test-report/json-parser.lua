-- Parses the NDJSON output produced by our busted output_handler into the
-- generic test_report.TestResult shape consumed by common/test-report.
--
-- Test id convention: "<abs_file_path>#<desc1::desc2::...::it_name>"

local log = require("utils.logging-util").new({
    name = "test-report-lua-parser",
    filename = "test-report.log",
    level = vim.log.levels.DEBUG,
})

local M = {}

local function safe_decode(line)
    if not line or line == "" then
        return nil
    end
    local ok, val = pcall(vim.json.decode, line, { luanil = { object = true, array = true } })
    if not ok then
        log.warn("decode failed for line: " .. line)
        return nil
    end
    return val
end

---@param status string Raw busted status: success|failure|error|pending
---@return "passed"|"failed"|"skipped"
local function map_status(status)
    if status == "success" then
        return "passed"
    end
    if status == "pending" then
        return "skipped"
    end
    return "failed" -- failure | error | anything unexpected
end

---@param dirs string[]
---@return table<string, test_report.TestResult>
function M.parse_results(dirs)
    local results = {} ---@type table<string, test_report.TestResult>
    if not dirs or #dirs == 0 then
        return results
    end

    for _, dir in ipairs(dirs) do
        local file_path = dir .. "/busted.ndjson"
        if vim.fn.filereadable(file_path) == 1 then
            for _, line in ipairs(vim.fn.readfile(file_path)) do
                local entry = safe_decode(line)
                if entry and entry.file and entry.descriptions then
                    local file = vim.fn.fnamemodify(entry.file, ":p")
                    -- Encode # in description to avoid collision with the
                    -- "<file>#<member>" id separator used by the common core.
                    -- Decoded back in lang/lua.lua id_to_display.
                    local member = table.concat(entry.descriptions, "::"):gsub("#", "%%23")
                    local id = file .. "#" .. member
                    local status = map_status(entry.status)

                    local errors = nil
                    if status == "failed" then
                        errors = { { message = (entry.message ~= "" and entry.message) or "test failed" } }
                    end

                    local invocation = {
                        name = member,
                        status = status,
                        time = tonumber(entry.duration) or 0,
                        stdout = nil,
                        stderr = nil,
                        stacktrace = entry.trace,
                        metadata = (entry.message ~= "" and entry.message) or nil,
                    }

                    local existing = results[id]
                    if existing then
                        -- Merge duplicate ids (re-runs / multiple invocations)
                        if status == "failed" then
                            existing.status = "failed"
                        end
                        existing.time = (existing.time or 0) + (invocation.time or 0)
                        if errors then
                            existing.errors = existing.errors or {}
                            for _, e in ipairs(errors) do
                                table.insert(existing.errors, e)
                            end
                        end
                        table.insert(existing.invocations, invocation)
                    else
                        results[id] = {
                            status = status,
                            errors = errors,
                            time = invocation.time,
                            invocations = { invocation },
                        }
                    end
                end
            end
        else
            log.debug("report file not found: " .. file_path)
        end
    end

    return results
end

return M
