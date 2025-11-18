local M = {}

local java_ts_util = require("utils.java.java-ts-util")
local spinner = require("utils.ui.spinner")
-- local root = vim.fn.getcwd()
local root = vim.fs.root(0, { ".git", "pom.xml", "mvnw", "gradlew", "build.gradle", "build.gradle.kts" })
local src_dir = root .. "/src/main/java/"
local test_dir = root .. "/src/test/java/"

local java_test_namespace = vim.api.nvim_create_namespace("java.test.namespace")

local function strip_ansi(s)
    if not s then
        return s
    end

    -- Remove ANSI CSI sequences: ESC [ digits ; digits ... letter
    return s:gsub("\27%[[0-9;]*[A-Za-z]", "")
end

local function is_file_exists(filepath)
    return vim.fn.filereadable(filepath) == 1
end

local function java_class_to_path(classname)
    local relative_path = classname:gsub("%.", "/") .. ".java"
    local full_path_src = src_dir .. relative_path
    if is_file_exists(full_path_src) then
        return full_path_src
    end
    return test_dir .. relative_path
end

local function parse_maven_output(text)
    local diagnostics = {}
    local current = nil

    local function push_frame(classname, method, msg, file, lnum)
        table.insert(diagnostics, {
            lnum = lnum - 1,
            col = 0,
            message = string.format("[%s.%s] %s", classname, method, msg),
            source = "maven-test",
            filename = file,
            severity = vim.diagnostic.severity.ERROR,
        })
    end

    local function push_current(current_error)
        if current_error then
            local base_msg = current_error.msg or "Test failure"
            for _, frame in ipairs(current_error.stack) do
                push_frame(current_error.class, current_error.method, base_msg, frame.file, frame.line)
            end
        end
    end

    -- iterate line-by-line
    for line in text:gmatch("[^\r\n]+") do
        -- Skip empty lines
        -- if line:match("^%s*$") then
        --     goto continue
        -- end

        --------------------------------------------------------------------
        -- 1️⃣ Detect test failure heade:[ERROR] class_path.method_name -- Time elapsed: 0.058 s <<< FAILURE!
        --------------------------------------------------------------------
        local class, method = line:match("^%[ERROR%]%s+([%w%._$]+)%.([%w_]+)%s+%-%-")
        if class and method then
            -- save prev in case presents
            push_current(current)

            current = {
                class = class,
                method = method,
                msg = "",
                stack = {},
            }
            goto continue
            -- print(class, method)
        end

        if not current or line:match("^%[ERROR%]%s") then
            -- print(line)
            goto continue
        end

        -- Next all belong to same error (message (can be multiline) or stack trace)

        if current then
            --------------------------------------------------------------------
            -- 3️⃣ Stack trace frame
            --------------------------------------------------------------------

            -- local line = "        at ua.serhii.application.test.utils.TestUtil.assertThat(TestUtil.java:10)"
            local classpath, methodname, filename, lnum =
                line:match("^%s*at ([%w%._$]+)%.([%w%._$]+)%(([%w%._$]+):(%d+)%)$")
            -- print(classpath, methodname, filename, lnum)
            -- print(file_path)

            if classpath then
                -- print(classpath, methodname, filename, lnum)
                local file_path = java_class_to_path(classpath)
                table.insert(current.stack, {
                    file = file_path,
                    line = tonumber(lnum),
                })
            else
                --------------------------------------------------------------------
                -- 2️⃣ Capture exception message (one or more lines)
                --------------------------------------------------------------------

                -- local exc = line:match("^([%w%.]+%w[:-].*)")
                -- -- local exc = line:match("^([%w%.]+Error[:-].*)")
                -- if exc then
                --     current.msg = current.msg .. exc
                -- end
                current.msg = current.msg .. line

                -- local line = "java.lang.AssertionExc: some error to note"
                -- local exc = line:match("^([%w%.]+%w[:-].*)")
                -- print(exc)
            end
        end

        --------------------------------------------------------------------
        -- 4️⃣ End of block (detected when next failure header appears or EOF)
        --------------------------------------------------------------------
        -- if current and line:match("^%[ERROR%] Tests run") then
        --     -- ignore summary lines
        --     goto continue
        -- end

        -- If we hit another header → flush previous
        -- local is_new_header = line:match("^%[ERROR%]%s+[%w%._$]+%.[%w_]+%s+%-%-")
        -- if current and is_new_header and (current.class ~= class or current.method ~= method) then
        --     -- flush previous failure
        --     local base_msg = current.msg or "Test failure"
        --     for _, frame in ipairs(current.stack) do
        --         push_frame(current.class, current.method, base_msg, frame.file, frame.line)
        --     end
        --     current = nil
        -- end

        ::continue::
    end

    -- add last if presents
    push_current(current)

    -- flush last failure if any
    -- if current then
    --     local base_msg = current.msg or "Test failure"
    --     for _, frame in ipairs(current.stack) do
    --         push_frame(current.class, current.method, base_msg, frame.file, frame.line)
    --     end
    -- end

    return diagnostics
end

local test_out = [[WARNING: A terminally deprecated method in sun.misc.Unsafe has been called
WARNING: sun.misc.Unsafe::staticFieldBase has been called by com.google.inject.internal.aop.HiddenClassDefiner (file:/home/serhii/.sdkman/candidates/maven/current/lib/guice-5
.1.0-classes.jar)
WARNING: Please consider reporting this to the maintainers of class com.google.inject.internal.aop.HiddenClassDefiner
WARNING: sun.misc.Unsafe::staticFieldBase will be removed in a future release
[ERROR] Tests run: 2, Failures: 2, Errors: 0, Skipped: 0, Time elapsed: 0.075 s <<< FAILURE! -- in ua.serhii.application.Tests1
[ERROR] ua.serhii.application.Tests1.testSomething1 -- Time elapsed: 0.053 s <<< FAILURE!
org.opentest4j.AssertionFailedError: 

expected: 1
 but was: 2
        at ua.serhii.application.Tests1.testSomething1(Tests1.java:17)
 
[ERROR] ua.serhii.application.Tests1.testSomething -- Time elapsed: 0.006 s <<< FAILURE!
org.opentest4j.AssertionFailedError: 

expected: 3
 but was: 4
        at ua.serhii.application.Tests1.testSomething(Tests1.java:11)

[ERROR] Tests run: 2, Failures: 2, Errors: 0, Skipped: 0, Time elapsed: 0.005 s <<< FAILURE! -- in ua.serhii.application.Tests2
[ERROR] ua.serhii.application.Tests2.testSomething2 -- Time elapsed: 0.001 s <<< FAILURE!
org.opentest4j.AssertionFailedError: 

expected: 5
 but was: 6
        at ua.serhii.application.Tests2.testSomething2(Tests2.java:12)

[ERROR] ua.serhii.application.Tests2.testSomething3 -- Time elapsed: 0.001 s <<< FAILURE!
java.lang.AssertionError: some error to note
        at ua.serhii.application.test.utils.TestUtil.assertThat(TestUtil.java:10)
        at ua.serhii.application.Tests2.testAssdrt(Tests2.java:23)
        at ua.serhii.application.Tests2.testSomething3(Tests2.java:18)

[ERROR] Failures: 
[ERROR]   Tests1.testSomething:11 
expected: 7
 but was: 8
[ERROR]   Tests1.testSomething1:17 
expected: "23"
 but was: "22"
[ERROR]   Tests2.testSomething2:12 
expected: 5
 but was: 4
[ERROR]   Tests2.testSomething3:18->testAssdrt:23 some error to note
[ERROR] Tests run: 4, Failures: 4, Errors: 0, Skipped: 0
[ERROR] Failed to execute goal org.apache.maven.plugins:maven-surefire-plugin:3.2.5:test (default-test) on project serhii-application: There are test failures.
[ERROR] 
[ERROR] Please refer to /home/serhii/serhii.home/git/tests/serhii-application/target/surefire-reports for the individual test results.
[ERROR] Please refer to dump files (if any exist) [date].dump, [date]-jvmRun[N].dump and [date].dumpstream.
[ERROR] -> [Help 1]
[ERROR] 
[ERROR] To see the full stack trace of the errors, re-run Maven with the -e switch.
[ERROR] Re-run Maven using the -X switch to enable full debug logging.
[ERROR] 
[ERROR] For more information about the errors and possible solutions, please read the following articles:
[ERROR] [Help 1] http://cwiki.apache.org/confluence/display/MAVEN/MojoFailureException

[Process exited 1]
]]

local function publish_maven_diagnostics(clean_text)
    local diags = parse_maven_output(clean_text)

    -- group by filename because vim.diagnostic.set needs per-buffer
    local grouped = {}

    for _, d in ipairs(diags) do
        grouped[d.filename] = grouped[d.filename] or {}
        table.insert(grouped[d.filename], d)
    end

    dd(grouped)
    for file, list in pairs(grouped) do
        local bufnr = vim.fn.bufadd(file)
        vim.fn.bufload(bufnr)
        -- diags = maven_util.dedupe_file_diagnstics(diags)
        vim.diagnostic.set(java_test_namespace, bufnr, list, {})
    end
    vim.cmd("Trouble diagnostics open")
end

dd(parse_maven_output(test_out))
--parse_maven_output(test_out)

-- publish_maven_diagnostics(test_out)

------------------------------------

-- Main runner
local function run_mvn_test_cmd(cmd_args)
    vim.notify("🚀 " .. table.concat(cmd_args, " "), vim.log.levels.INFO)
    spinner.start("🚀 " .. table.concat(cmd_args, " "))

    vim.cmd("botright split")
    vim.cmd("resize 15")
    local term_win = vim.api.nvim_get_current_win()
    local term_buf = vim.api.nvim_create_buf(false, true)
    vim.api.nvim_win_set_buf(term_win, term_buf)

    local output = {}

    local job_id = vim.fn.jobstart(cmd_args, {
        term = true, -- the modern replacement for termopen()
        stdout_buffered = false,
        stderr_buffered = false,

        on_stdout = function(_, data)
            for _, l in ipairs(data) do
                if l ~= "" then
                    table.insert(output, l)
                end
            end
        end,
        on_stderr = function(_, data)
            for _, l in ipairs(data) do
                if l ~= "" then
                    table.insert(output, l)
                end
            end
        end,
        on_exit = function(_, code)
            spinner.stop(code == 0, "Maven tests")
            local all = table.concat(output, "\n")
            all = strip_ansi(all)
            vim.notify(all, vim.log.levels.INFO)
            -- local diags = parse_maven_output(all)
            -- publish_diagnostics(diags)
            publish_maven_diagnostics(all)

            -- if #diags > 0 then
            --     print("Maven finished with test failures → diagnostics added.")
            -- else
            --     print("Maven finished successfully, no diagnostics.")
            -- end
            -- if mvn_term.win and vim.api.nvim_win_is_valid(mvn_term.win) then
            --     vim.api.nvim_win_close(mvn_term.win, true)
            --     mvn_term.win = nil
            -- end
            -- SUCCESS → close *only this split window*
            if code == 0 and vim.api.nvim_win_is_valid(term_win) then
                vim.api.nvim_win_close(term_win, true)
            end
        end,
    })

    vim.cmd("startinsert")
    if job_id <= 0 then
        vim.notify("Failed to start mvn verify via jobstart()", vim.log.levels.ERROR)
    end
end

local function get_test_runner(test_name)
    return { "mvn", "-q", "test", "-Dtest=" .. test_name }
end

local function get_verify_runner()
    return { "mvn", "-q", "verify", "-DskipAssembly", "-DskipInstall", "-DskipTests=false" }
end

M.run_java_test_method = function()
    local method_name = java_ts_util.get_full_method("#")
    if method_name then
        run_mvn_test_cmd(get_test_runner(method_name))
    else
        vim.notify("❌ Could not determine current method name", vim.log.levels.WARN)
    end
end

M.run_java_test_class = function()
    local class_name = java_ts_util.get_class_name()
    if class_name then
        run_mvn_test_cmd(get_test_runner(class_name))
    else
        vim.notify("❌ Could not determine current class name", vim.log.levels.WARN)
    end
end

M.run_java_test_all = function()
    run_mvn_test_cmd(get_verify_runner())
end

return M
