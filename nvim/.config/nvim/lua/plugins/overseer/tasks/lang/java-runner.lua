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

-- Test types whose target is the file/method under the cursor. Only these are
-- guarded against a run/test key mix-up; project-wide types (ALL_TESTS, module
-- runs, TOGGLE_LAST_DEBUG) are not tied to the current buffer.
local FILE_BOUND_TEST_TYPES = {
    [task.test_type.FILE_TESTS] = true,
    [task.test_type.CURRENT_TEST] = true,
    [task.test_type.CURRENT_PARAMETRIZED_NUM_TEST] = true,
}

---@return boolean
local function is_test_context()
    local java_ts = require("utils.java.java-ts-util")
    -- src/test path is the strongest signal; treesitter annotations cover the
    -- (rare) test class kept outside a test source root.
    return require("utils.java.java-common").is_test_file()
        or java_ts.is_test_method_at_cursor()
        or java_ts.class_has_test_methods()
end

-- Guard against the common <leader>r… / <leader>t… mix-up. Running a test class
-- as a `main` (or debugging one) launches a JVM with no entry point and can leave
-- a half-attached DAP session that needs a full nvim restart to clear, so we
-- refuse the wrong key up front rather than letting it break the session.
---@type task.lang.RunGuard
M.run_guard = {
    -- Gate <leader>r… (run/debug current as a main class).
    can_run = function()
        if is_test_context() then
            return false, "✋ This is a test — use <leader>t… (test runner), not <leader>r…"
        end
        return true
    end,
    -- Gate <leader>t… (run/debug the current file's tests).
    ---@param context task.lang.Context
    can_test = function(context)
        if not FILE_BOUND_TEST_TYPES[context.test_type] then
            return true
        end
        if is_test_context() then
            return true
        end
        local java_ts = require("utils.java.java-ts-util")
        if java_ts.has_main_method() then
            return false, "✋ This is a main class — use <leader>r… (run/debug), not <leader>t…"
        end
        return false, "✋ Current file is not a test — use <leader>r… (run/debug), not <leader>t…"
    end,
}

return M