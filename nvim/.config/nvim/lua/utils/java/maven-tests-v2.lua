local M = {}

local java_test_namespace = vim.api.nvim_create_namespace("java.test.namespace")
local java_ts_util = require("utils.java.java-ts-util")
local util = require("utils.common-util")
local java_util = require("utils.java.java-common")
local spinner = require("utils.ui.spinner")
local java_trace = require("utils.java.java-trace")
local last_runned_test_cmd_args = nil
local current_term_win = nil

local function push_frame(diagnostics_table, classname, method, msg, number, frame_number, file, lnum)
    table.insert(diagnostics_table, {
        lnum = lnum - 1,
        col = 0,
        -- message = string.format("[%s.%s] %s", classname, method, msg),
        message = string.format("(%s-%s): %s", number, frame_number, msg),
        -- "Bold text in nvim term\e[0m\n"
        source = "maven-test",
        filename = file,
        severity = vim.diagnostic.severity.ERROR,
        class_name = classname,
        method = method,
    })
end

local function push_current(diagnostics_table, current_error)
    if current_error then
        local base_msg = current_error.msg or "Test failure"
        -- for index, frame in ipairs(current_error.stack) do
        local frame_number = 1
        for i = #current_error.stack, 1, -1 do
            local frame = current_error.stack[i]
            push_frame(
                diagnostics_table,
                current_error.class,
                current_error.method,
                base_msg,
                current_error.number,
                frame_number,
                frame.file,
                frame.line
            )
            frame_number = frame_number + 1
        end
    end
end

M.parse_maven_output = function(text)
    local diagnostics = {}
    local current = nil
    local current_num = 1

    -- iterate line-by-line
    for line in text:gmatch("[^\r\n]+") do
        --------------------------------------------------------------------
        -- Detect test failure heade:[ERROR] class_path.method_name -- Time elapsed: 0.058 s <<< FAILURE!
        --------------------------------------------------------------------
        local class, method = line:match("^%[ERROR%]%s+([%w%._$]+)%.([%w_]+)%s+%-%-")
        if class and method then
            -- save prev in case presents
            push_current(diagnostics, current)

            current = {
                class = class,
                method = method,
                msg = nil,
                stack = {},
                number = current_num,
            }
            current_num = current_num + 1
            goto continue
            -- print(class, method)
        end

        if current and line:match("^%[ERROR%] Failures:%s*$") then
            break
        end

        if not current or line:match("^%[ERROR%]%s") then
            -- print(line)
            goto continue
        end

        -- Next all belong to same error (message (can be multiline) or stack trace)
        if current then
            --------------------------------------------------------------------
            -- Stack trace frame
            --------------------------------------------------------------------

            -- local line = "        at ua.serhii.application.test.utils.TestUtil.assertThat(TestUtil.java:10)"
            -- local classpath, methodname, filename, lnum =
            --     line:match("^%s*at ([%w%._$]+)%.([%w%._$]+)%(([%w%._$]+):(%d+)%)$")
            local trace = java_util.parse_java_mvn_run_class_line(line)
            -- print(classpath, methodname, filename, lnum)
            -- print(file_path)

            if trace then
                -- print(classpath, methodname, filename, lnum)
                local file_path = java_util.java_class_to_proj_path(trace.class_path)
                -- dd({ trace = trace, path = file_path })

                if file_path then
                    table.insert(current.stack, {
                        file = file_path,
                        line = tonumber(trace.class_line_number),
                    })
                else
                    -- TODO: jdtls lib qflist
                end
            else
                --------------------------------------------------------------------
                -- Capture exception message (one or more lines)
                --------------------------------------------------------------------

                current.msg = current.msg and current.msg .. "\n" .. line or line
                -- print(exc)
            end
        end

        ::continue::
    end

    -- add last if presents
    push_current(diagnostics, current)

    return diagnostics
end

M.publish_maven_diagnostics = function(clean_text)
    local diags = M.parse_maven_output(clean_text)
    if vim.tbl_isempty(diags) then
        vim.diagnostic.reset()
        return
    end

    -- group by filename because vim.diagnostic.set needs per-buffer
    local grouped = {}

    for _, d in ipairs(diags) do
        grouped[d.filename] = grouped[d.filename] or {}
        table.insert(grouped[d.filename], d)
    end

    -- dd(grouped)
    vim.diagnostic.reset()
    for file, list in pairs(grouped) do
        local bufnr = vim.fn.bufadd(file)
        vim.fn.bufload(bufnr)
        -- diags = maven_util.dedupe_file_diagnstics(diags)
        vim.diagnostic.set(java_test_namespace, bufnr, list, {})
    end

    -- Future planning:
    -- TODO: print number of failed tests
    -- TODO: virtual marks on opened buffers with failed tests

    -- vim.notify("❌😬 Test Finished with Fails", vim.log.levels.WARN)
    vim.cmd("Trouble diagnostics open")
end

-- Main runner
local function run_mvn_test_cmd(cmd_args)
    vim.notify("🚀 " .. table.concat(cmd_args, " "), vim.log.levels.INFO)
    vim.notify("cwd: " .. vim.fn.getcwd(), vim.log.levels.INFO)
    spinner.start("🚀 " .. table.concat(cmd_args, " "))

    vim.cmd("botright split")
    -- vim.cmd("resize 15")

    util.close_window_if_exists(current_term_win) -- close prev term win if opened
    current_term_win = vim.api.nvim_get_current_win()
    local term_buf = vim.api.nvim_create_buf(false, true)
    vim.api.nvim_win_set_buf(current_term_win, term_buf)

    last_runned_test_cmd_args = cmd_args
    local output = {}
    local job_id = vim.fn.jobstart(cmd_args, {
        cwd = vim.fn.getcwd(),
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
            all = util.strip_ansi(all)
            -- vim.notify(all, vim.log.levels.INFO)
            M.publish_maven_diagnostics(all)

            ------------------------------------
            -- java_trace.highlight_java_test_trace(term_buf)
            ------------------------------------

            if code == 0 then
                util.close_window_if_exists(current_term_win)
                -- vim.notify("✅ 💪 Test Successfully Passed", vim.log.levels.INFO)
            end
        end,
    })

    vim.cmd("startinsert")
    if job_id <= 0 then
        vim.notify("Failed to start mvn verify via jobstart()", vim.log.levels.ERROR)
    end
end

local function get_test_runner(test_name, is_debug)
    if is_debug then
        return { "mvn", "-q", "test", "-Dmaven.surefire.debug", "-Dtest=" .. test_name }
    end
    return { "mvn", "-q", "test", "-Dtest=" .. test_name }
end

local function get_verify_runner()
    return { "mvn", "-q", "verify", "-DskipAssembly", "-DskipInstall", "-DskipTests=false" }
end

M.run_java_test_method = function(is_debug)
    local method_name = java_ts_util.get_full_method("#")
    if method_name then
        run_mvn_test_cmd(get_test_runner(method_name, is_debug))
    else
        vim.notify("❌ Could not determine current method name", vim.log.levels.WARN)
    end
end

M.run_java_test_class = function(is_debug)
    local class_name = java_ts_util.get_class_name()
    if class_name then
        run_mvn_test_cmd(get_test_runner(class_name, is_debug))
    else
        vim.notify("❌ Could not determine current class name", vim.log.levels.WARN)
    end
end

M.run_java_test_all = function()
    run_mvn_test_cmd(get_verify_runner())
end

M.rerun_last_cmd = function()
    if last_runned_test_cmd_args then
        run_mvn_test_cmd(last_runned_test_cmd_args)
    else
        vim.notify("⚠️ No previous mvn test cmd to run", vim.log.levels.WARN)
    end
end

return M
