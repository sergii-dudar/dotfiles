local java_bin = vim.fn.glob("~/.sdkman/candidates/java/current/bin/java")

local M = {}

---@return table<string, string>
function M.get_envs()
    local java_util = require("utils.java.java-common")
    local module_path = java_util.get_buffer_project_path()
    if not module_path then
        return {}
    end
    local env_file = module_path .. "/src/main/resources/dev.application.properties.env"
    return require("utils.envs-util").load_env_file(env_file)
end

---@return table
function M.build_run_cmd()
    return build_java_cmd()
end

---@return table
function M.build_debug_cmd()
    return build_java_cmd(true)
end

---@param is_debug boolean|nil
function build_java_cmd(is_debug)
    local classpath = require("utils.java.jdtls-classpath-util").get_classpath_for_main_method()
    local class_name = require("utils.java.java-ts-util").get_class_name()
    if not class_name then
        vim.notify("❌ Could not determine current class name for debug", vim.log.levels.WARN)
        return {}
    end

    if not is_debug then
        return {
            java_bin,
            "--enable-native-access=ALL-UNNAMED",
            "-cp",
            classpath,
            class_name,
        }
    end

    return {
        java_bin,
        "--enable-native-access=ALL-UNNAMED",
        "-agentlib:jdwp=transport=dt_socket,server=y,suspend=y,address=5005",
        "-cp",
        classpath,
        class_name,
    }
end

--- Attach to existing jvm dap session.
---@param port integer|nil 5005 if not specified.
function M.dap_attach_to_remote(port)
    require("utils.java.jdtls-config-dap-util").attach_to_remote(port)
end

--- Output-driven DAP attach for the overseer debug-task flow. The JVM is launched
--- with `-agentlib:jdwp=...,suspend=y` and prints a "Listening for transport..."
--- banner; debug/dap_ctrl_component scans output and calls this once it appears.
---@type task.lang.DapOutputAttacher
M.dap_output_attacher = {
    name = "jdwp",
    match = function(line)
        return line:match("Listening for transport dt_socket at address: (%d+)")
    end,
    attach = function(port)
        vim.notify("Connecting to java dap port: " .. port)
        require("utils.java.jdtls-config-dap-util").attach_to_remote(tonumber(port))
    end,
}

function M.dap_launch()
    require("utils.java.jdtls-config-dap-util").run_current_main_class()
end

function M.dap_launch_rerun()
    require("utils.java.jdtls-config-dap-util").rerun_last()
end

---@param context task.lang.Context
---@return task.lang.test.TestCmd
function M.build_run_test_cmd(context)
    return require("modules.java.junit.init").build_run_test_cmd(context)
end

function M.get_test_report_dir()
    local java_util = require("utils.java.java-common")
    return java_util.get_build_layout(java_util.get_buffer_project_path()).report_dir
end

-- Test types whose target is the method under the cursor. Only these require the
-- cursor to be inside a JUnit method; file-wide/project-wide test commands have a
-- wider target.
local CURSOR_BOUND_TEST_TYPES = {
    [task.test_type.CURRENT_TEST] = true,
    [task.test_type.CURRENT_PARAMETRIZED_NUM_TEST] = true,
}

--- Whether the current Java file/class should be treated as a JUnit test target.
---@return boolean
local function is_test_file_context()
    local java_ts = require("utils.java.java-ts-util")
    -- src/test path is the strongest signal; treesitter annotations cover the
    -- (rare) test class kept outside a test source root.
    return require("utils.java.java-common").is_test_file() or java_ts.class_has_test_methods()
end

--- Decide whether the current Java method can be run through the test runner.
---@return boolean, string|nil
local function can_test_current_method()
    local java_ts = require("utils.java.java-ts-util")
    if java_ts.is_test_method_at_cursor() then
        return true
    end
    if java_ts.is_main_method_at_cursor() then
        return false, "✋ Cursor is in main method — use <leader>r… (run/debug), not <leader>t…"
    end
    if is_test_file_context() then
        return false, "✋ Cursor is not in a test method"
    end
    if java_ts.has_main_method() then
        return false, "✋ This is a main class — use <leader>r… (run/debug), not <leader>t…"
    end
    return false, "✋ Current file is not a test — use <leader>r… (run/debug), not <leader>t…"
end

--- Decide whether the current Java file can be run through the test runner.
---@return boolean, string|nil
local function can_test_current_file()
    local java_ts = require("utils.java.java-ts-util")
    if java_ts.is_main_method_at_cursor() then
        return false, "✋ Cursor is in main method — use <leader>r… (run/debug), not <leader>t…"
    end
    if is_test_file_context() then
        return true
    end
    if java_ts.has_main_method() then
        return false, "✋ This is a main class — use <leader>r… (run/debug), not <leader>t…"
    end
    return false, "✋ Current file is not a test — use <leader>r… (run/debug), not <leader>t…"
end

-- Guard against the common <leader>r… / <leader>t… mix-up. Test classes can also
-- contain ad-hoc `main` methods, so mixed files are resolved by cursor position:
-- JUnit methods go through the test runner, and `main` goes through run/debug.
---@type task.lang.RunGuard
M.run_guard = {
    -- Gate <leader>r… (run/debug current as a main class).
    --- Decide whether the current Java file can be run as a main class.
    can_run = function()
        local java_ts = require("utils.java.java-ts-util")
        if not is_test_file_context() then
            return true
        end
        if java_ts.is_main_method_at_cursor() then
            return true
        end
        if java_ts.is_test_method_at_cursor() then
            return false, "✋ This is a test — use <leader>t… (test runner), not <leader>r…"
        end
        return false, "✋ Cursor is in a test class — move to main method or use <leader>t…"
    end,
    -- Gate <leader>t… (run/debug the current file's tests).
    --- Decide whether the selected Java test command is valid at the cursor.
    ---@param context task.lang.Context
    can_test = function(context)
        if CURSOR_BOUND_TEST_TYPES[context.test_type] then
            return can_test_current_method()
        end
        if context.test_type == task.test_type.FILE_TESTS then
            return can_test_current_file()
        end
        return true
    end,
}

return M
