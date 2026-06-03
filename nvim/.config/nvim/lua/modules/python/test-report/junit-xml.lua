-- pytest JUnit XML parser.
--
-- pytest's report shape (via `--junit-xml=<file>`):
--   <testsuites>
--     <testsuite name="pytest" tests=N failures=N errors=N skipped=N time="..">
--       <testcase classname="tests.test_foo.TestBar" file="tests/test_foo.py"
--                 line="19" name="test_method[param-1]" time="..">
--         <failure message="...">long traceback...</failure>?
--         <error message="..">collection error...</error>?
--         <skipped message="..">skip reason</skipped>?
--         <system-out>..</system-out>?
--         <system-err>..</system-err>?
--       </testcase>
--       ...
--     </testsuite>
--   </testsuites>
--
-- `classname` is the dotted python module path. If the test is inside a class,
-- the LAST dotted segment is the class name. Otherwise it's the module path.
-- We split on the first capital-letter segment to recover the class.
--
-- Parametrized tests share a base name; `name` carries the `[param-id]`
-- suffix. We strip it and aggregate invocations.

local xml = require("lib.xml")
local file = require("lib.file")
local log = require("utils.logging-util").new({
    name = "test-report-python-xml",
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

function M.clear_cache()
    -- nothing cached at the moment
end

---@param project_root string
---@return string|nil
local function project_root()
    local ok, runner = pcall(require, "modules.python.pytest-test")
    if ok and runner.project_root then
        return runner.project_root()
    end
    return nil
end

--- Resolve the absolute file path for a testcase.
--- pytest only sometimes emits `file=`. When missing, derive from `classname`
--- (dotted python module path with optional trailing TestClass segment).
---@param file_attr string|nil  testcase[file] attr (may be empty)
---@param module_path string|nil  dotted module path (classname minus class segment)
---@return string  absolute file path (or empty string if unresolvable)
local function resolve_file(file_attr, module_path)
    local root = project_root()

    -- 1) Honour explicit `file=` attr if present.
    if file_attr and file_attr ~= "" then
        if file_attr:sub(1, 1) == "/" then
            return file_attr
        end
        if root then
            local p = root .. "/" .. file_attr
            if vim.fn.filereadable(p) == 1 then
                return p
            end
        end
        local abs = vim.fn.fnamemodify(file_attr, ":p")
        if vim.fn.filereadable(abs) == 1 then
            return abs
        end
    end

    -- 2) Derive from dotted module path: foo.bar.baz -> foo/bar/baz.py
    if module_path and module_path ~= "" and root then
        local rel = module_path:gsub("%.", "/") .. ".py"
        local p = root .. "/" .. rel
        if vim.fn.filereadable(p) == 1 then
            return p
        end
        -- Some pytest configs add a `pythonpath` (e.g. `src/`). Walk likely
        -- src dirs to find the file.
        for _, prefix in ipairs({ "src", "lib", "app" }) do
            local pp = root .. "/" .. prefix .. "/" .. rel
            if vim.fn.filereadable(pp) == 1 then
                return pp
            end
        end
        -- Last-ditch: search the project for matching basename.
        local basename = vim.fn.fnamemodify(rel, ":t")
        local found = vim.fn.globpath(root, "**/" .. basename, false, true)
        if type(found) == "table" and #found > 0 then
            return found[1]
        end
    end

    return ""
end

--- Extract the class name from a pytest classname, or nil if module-level.
--- Examples:
---   "tests.test_assertions"                      -> nil
---   "tests.test_classes.TestBankAccount"         -> "TestBankAccount"
---   "tests.test_classes.TestBankAccount.Nested"  -> "TestBankAccount.Nested"  (rare; pytest joins)
---@param classname string
---@return string|nil
local function extract_class(classname)
    if not classname or classname == "" then
        return nil
    end
    -- Walk segments; the class is the first segment whose first letter is uppercase.
    local segs = {}
    for s in (classname .. "."):gmatch("(.-)%.") do
        table.insert(segs, s)
    end
    local class_parts = {}
    for _, s in ipairs(segs) do
        if s ~= "" then
            local first = s:sub(1, 1)
            if first >= "A" and first <= "Z" then
                table.insert(class_parts, s)
            elseif #class_parts > 0 then
                -- a lowercase segment after capitalized ones — unusual; stop.
                break
            end
        end
    end
    if #class_parts == 0 then
        return nil
    end
    return table.concat(class_parts, ".")
end

--- Strip parametrize `[...]` suffix off a test name.
---@param name string
---@return string base_name, string|nil param_id
local function split_param(name)
    local base, param = name:match("^(.-)%[(.+)%]$")
    if base then
        return base, param
    end
    return name, nil
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
    if message == "" and stacktrace ~= "" then
        -- first non-empty line
        for line in stacktrace:gmatch("[^\n]+") do
            local trimmed = line:gsub("^%s+", ""):gsub("%s+$", "")
            if trimmed ~= "" then
                message = trimmed
                break
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
    -- pytest traceback line shape: "tests/test_foo.py:42: in test_x"
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
        vim.notify("test-report: failed to parse pytest XML: " .. filepath, vim.log.levels.ERROR)
        return results
    end
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
                    local classname = attr.classname or ""
                    local class_name = extract_class(classname)
                    local module_path = classname
                    if class_name and class_name ~= "" then
                        -- strip ".ClassName" (or ".ClassName.Sub") suffix
                        module_path = classname:sub(1, #classname - #class_name - 1)
                    end
                    local abs_file = resolve_file(file_attr, module_path)
                    if abs_file ~= "" then
                        local raw_name = attr.name or ""
                        local base_name, param_id = split_param(raw_name)
                        local member
                        if class_name then
                            member = class_name .. "." .. base_name
                        else
                            member = base_name
                        end
                        -- ID = <abs_file>#<member>. Member is "Class.method" for
                        -- class-based tests, just the function name otherwise.
                        -- The shared test-report core splits ids on the first `#`.
                        local id = abs_file .. "#" .. member
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
                            name = param_id and (base_name .. "[" .. param_id .. "]") or base_name,
                            status = status,
                            stdout = extract_text(tc["system-out"]),
                            stderr = extract_text(tc["system-err"]),
                            stacktrace = stacktrace,
                            time = time,
                        }

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
        end
    end
    return results
end

return M
