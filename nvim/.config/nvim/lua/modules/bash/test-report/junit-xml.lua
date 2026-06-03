-- bashunit JUnit XML parser.
--
-- bashunit's report shape (per `--log-junit <file>`):
--   <testsuites>
--     <testsuite name="bashunit" tests="N" failures="N" errors="N" skipped="N" time="..">
--       <testcase file="tests/foo_test.sh" name="<humanized name>" time="..">
--         <failure message="..">..stderr/details..</failure>?
--       </testcase>
--       ...
--     </testsuite>
--   </testsuites>
--
-- "name" is the *humanized* form of the test function name (strip `test_`,
-- replace `_` with space, capitalize first letter). To recover stable ids
-- of the form `<abs_file>#test_<fn>`, we walk each file's treesitter tree
-- and build a humanized->fn map.
--
-- For data providers, the same humanized name appears multiple times — we
-- aggregate into a single TestResult with one invocation per occurrence
-- (mirrors the Java parameterized path).

local xml = require("lib.xml")
local file = require("lib.file")
local log = require("utils.logging-util").new({
    name = "test-report-bash-xml",
    filename = "test-report.log",
    level = vim.log.levels.DEBUG,
})

local M = {}

---@param report_dir string
---@return string[]
function M.list_report_files(report_dir)
    log.debug("list_report_files: " .. report_dir)
    -- bashunit writes a single file (we configure it to junit.xml), but accept any .xml
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

local function extract_text(node)
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

--- Apply the same humanization bashunit uses on test function names.
--- See bashunit/src/helpers.sh::normalize_test_function_name.
---@param fn_name string
---@return string
function M.humanize(fn_name)
    local rest = fn_name:gsub("^test_", "")
    if rest == fn_name then
        rest = fn_name:gsub("^test", "")
    end
    rest = rest:gsub("_", " ")
    local first = rest:sub(1, 1)
    if first ~= "" then
        rest = first:upper() .. rest:sub(2)
    end
    return rest
end

local _query
local function bash_test_query()
    if _query then
        return _query
    end
    _query = vim.treesitter.query.parse(
        "bash",
        [[
        (function_definition
          name: (word) @fn.name
          (#match? @fn.name "^test_?")
        )
    ]]
    )
    return _query
end

-- file_path -> { humanized -> fn_name }; refreshed on mtime change.
local _humanized_cache = {}

---@param file_path string
---@return table<string, string>
local function humanized_to_fn(file_path)
    local mtime = vim.fn.getftime(file_path)
    local cached = _humanized_cache[file_path]
    if cached and cached.mtime == mtime then
        return cached.map
    end
    local map = {}
    if vim.fn.filereadable(file_path) ~= 1 then
        _humanized_cache[file_path] = { mtime = mtime, map = map }
        return map
    end
    local bufnr = vim.fn.bufadd(file_path)
    if not vim.api.nvim_buf_is_loaded(bufnr) then
        pcall(vim.cmd, "noautocmd call bufload(" .. bufnr .. ")")
    end
    local ok, parser = pcall(vim.treesitter.get_parser, bufnr, "bash")
    if not ok or not parser then
        _humanized_cache[file_path] = { mtime = mtime, map = map }
        return map
    end
    local tree = parser:parse()[1]
    if not tree then
        _humanized_cache[file_path] = { mtime = mtime, map = map }
        return map
    end
    local query = bash_test_query()
    for id, node in query:iter_captures(tree:root(), bufnr) do
        if query.captures[id] == "fn.name" then
            local fn = vim.treesitter.get_node_text(node, bufnr)
            map[M.humanize(fn)] = fn
        end
    end
    _humanized_cache[file_path] = { mtime = mtime, map = map }
    return map
end

function M.clear_cache()
    _humanized_cache = {}
end

---@param file_attr string  the value of testcase[file=...]
---@param report_dir string
---@return string  absolute file path
local function resolve_file(file_attr, report_dir)
    if file_attr:sub(1, 1) == "/" then
        return file_attr
    end
    -- Try project_root first (report_dir is .../test-report/bash/<safe_root>/)
    local ok, runner = pcall(require, "modules.bash.bashunit-test")
    if ok and runner.project_root then
        local root = runner.project_root()
        local p = root:gsub("/$", "") .. "/" .. file_attr
        if vim.fn.filereadable(p) == 1 then
            return p
        end
    end
    -- Fallback: cwd
    return vim.fn.fnamemodify(file_attr, ":p")
end

---@param failure_node table|string
---@return string message, string stacktrace
local function extract_failure(failure_node)
    local message, stacktrace = "", ""
    local failures = failure_node
    if type(failures) ~= "table" or failures._attr or type(failures[1]) == "string" then
        failures = { failure_node }
    end
    for _, f in ipairs(failures) do
        if type(f) == "table" then
            if f._attr and f._attr.message and message == "" then
                message = f._attr.message
            end
            local text = extract_text(f)
            if text then
                stacktrace = stacktrace .. (stacktrace == "" and "" or "\n") .. text
            end
        elseif type(f) == "string" then
            stacktrace = stacktrace .. (stacktrace == "" and "" or "\n") .. f
        end
    end
    -- bashunit always sets message="Test failed" — extract a better message
    -- from the stacktrace body (first useful non-empty line) when available.
    if message == "" or message == "Test failed" then
        -- Collect consecutive non-header, non-frame lines into a single message.
        local collected = {}
        for line in stacktrace:gmatch("[^\n]+") do
            local trimmed = line:gsub("^%s+", ""):gsub("%s+$", "")
            if trimmed == "" or trimmed:match("^✗") then
                if #collected > 0 then
                    break
                end
            elseif trimmed:match("^at%s+") then
                break
            else
                collected[#collected + 1] = trimmed
            end
        end
        if #collected > 0 then
            message = table.concat(collected, " ")
        end
        if message == "" then
            -- last resort: first non-empty line, even if header
            for line in stacktrace:gmatch("[^\n]+") do
                local trimmed = line:gsub("^%s+", ""):gsub("%s+$", "")
                if trimmed ~= "" then
                    message = trimmed
                    break
                end
            end
        end
    end
    return message, stacktrace
end

---@param file_path string
---@param stacktrace string
---@return number|nil
local function extract_error_line(file_path, stacktrace)
    if not stacktrace or stacktrace == "" then
        return nil
    end
    local basename = vim.fn.fnamemodify(file_path, ":t")
    local lnum = stacktrace:match(vim.pesc(file_path) .. ":(%d+)")
    if lnum then
        return tonumber(lnum)
    end
    lnum = stacktrace:match(vim.pesc(basename) .. ":(%d+)")
    if lnum then
        return tonumber(lnum)
    end
    return nil
end

---@param filepath string  path to junit.xml
---@param report_dir string
---@return table<string, test_report.TestResult>
function M.parse_file(filepath, report_dir)
    local results = {}
    log.debug("parsing: " .. filepath)
    local content = file.read_file(filepath)
    if not content or content == "" then
        return results
    end
    local ok, parsed = pcall(xml.parse, content)
    if not ok or type(parsed) ~= "table" then
        log.error("XML parse error: " .. tostring(parsed))
        vim.notify("test-report: failed to parse bashunit XML: " .. filepath, vim.log.levels.ERROR)
        return results
    end
    -- Root may be <testsuites> wrapping one <testsuite>, OR a bare <testsuite>.
    local suites = parsed.testsuites and parsed.testsuites.testsuite or parsed.testsuite
    if not suites then
        return results
    end
    if suites._attr then
        suites = { suites }
    end

    for _, suite in ipairs(suites) do
        local cases = suite.testcase
        if cases then
            if cases._attr then
                cases = { cases }
            end
            for _, tc in ipairs(cases) do
                local attr = tc._attr
                if attr then
                    local file_attr = attr.file or ""
                    local humanized = attr.name or ""
                    local abs_file = resolve_file(file_attr, report_dir)
                    -- Map humanized -> fn name via treesitter; fall back to humanized.
                    local map = humanized_to_fn(abs_file)
                    local fn_name = map[humanized] or humanized
                    local id = abs_file .. "#" .. fn_name
                    local time = tonumber(attr.time)

                    local status, errors, stacktrace
                    if tc.failure then
                        local msg, st = extract_failure(tc.failure)
                        stacktrace = st
                        errors = { { message = msg, line = extract_error_line(abs_file, st) } }
                        status = "failed"
                    elseif tc.error then
                        local msg, st = extract_failure(tc.error)
                        stacktrace = st
                        errors = { { message = msg, line = extract_error_line(abs_file, st) } }
                        status = "failed"
                    elseif tc.skipped then
                        status = "skipped"
                    else
                        status = "passed"
                    end

                    local invocation = {
                        name = humanized,
                        status = status,
                        stdout = extract_text(tc["system-out"]),
                        stderr = extract_text(tc["system-err"]),
                        stacktrace = stacktrace,
                        time = time,
                    }

                    local existing = results[id]
                    if existing then
                        -- Aggregate data-provider invocations.
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
    end
    return results
end

return M
