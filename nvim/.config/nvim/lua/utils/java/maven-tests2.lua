local M = {}

local java_ts_util = require("utils.java.java-ts-util")
local spinner = require("utils.ui.spinner")

local java_namespace = vim.api.nvim_create_namespace("java.compile.namespace")

local function strip_ansi(s)
    if not s then
        return s
    end

    -- Remove ANSI CSI sequences: ESC [ digits ; digits ... letter
    return s:gsub("\27%[[0-9;]*[A-Za-z]", "")
end

-- Parse mvn verify output for test failures
-- Supports multiline failure blocks for surefire/failsafe
local function parse_maven_output(output)
    local diagnostics = {}

    -- Typical Surefire failure block:
    -- -------------------------------------------------------
    --  Failed tests:
    --    SomeTest.shouldDoThing:42 expected:<1> but was:<2>
    --    MultiLineTest.testX:17
    --    Expected: hello
    --    Actual:   world
    -- -------------------------------------------------------

    local current_entry = nil
    for line in output:gmatch("[^\r\n]+") do
        -- Start of a failure line
        local class, testname, lineno, after = line:match("^%s*([%w%._$]+)%.([%w_]+):(%d+)%s*(.*)")
        if class then
            -- If we were parsing an existing block, push it
            if current_entry then
                table.insert(diagnostics, current_entry)
            end

            current_entry = {
                file = class:gsub("%.", "/") .. ".java",
                lnum = tonumber(lineno) - 1,
                message = testname .. ": " .. after,
            }
        else
            -- Continuation of multiline block
            if current_entry then
                current_entry.message = current_entry.message .. "\n" .. line
            end
        end
    end

    -- Final
    if current_entry then
        table.insert(diagnostics, current_entry)
    end

    return diagnostics
end

-- Put diagnostics into workspace
local function publish_diagnostics(diags)
    local namespace = vim.api.nvim_create_namespace("mvn_verify")
    local cwd = vim.loop.cwd()

    for _, d in ipairs(diags) do
        local filepath = cwd .. "/src/test/java/" .. d.file
        vim.diagnostic.set(namespace, vim.uri_to_bufnr(vim.uri_from_fname(filepath)), {
            {
                lnum = d.lnum,
                col = 0,
                severity = vim.diagnostic.severity.ERROR,
                message = d.message,
                source = "mvn verify",
            },
        })
    end
end

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

    -- local job_id = vim.fn.jobstart({ "mvn", unpack(cmd_args) }, {
    local job_id = vim.fn.jobstart(cmd_args, {
        term = true, -- the modern replacement for termopen()
        stdout_buffered = false,
        stderr_buffered = false,
        -- vim.fn.termopen("mvn -q verify", {

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
            local diags = parse_maven_output(all)
            publish_diagnostics(diags)

            if #diags > 0 then
                print("Maven finished with test failures → diagnostics added.")
            else
                print("Maven finished successfully, no diagnostics.")
            end
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
