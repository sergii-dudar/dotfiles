local java_util = require("utils.java.java-common")
local java_ts = require("utils.java.java-ts-util")
local javap_util = require("utils.java.javap-util")
local jdtls_util = require("utils.java.jdtls-util")
local string_util = require("utils.string-util")
local nio_util = require("utils.nio-util")

local M = {}

local setting = {
    junit_jar = vim.fn.glob("$HOME/tools/java-extensions/junit/junit-platform-console-standalone.jar"),
    jvm_args = {
        string.format("-javaagent:%s/tools/java-extensions/jmockit/jmockit.jar", os.getenv("HOME")),
    },
    report_dir = "/target/junit-report",
}
local state = {
    parametrized_test_num = 0,
}

vim.api.nvim_create_user_command("ParamTestNum", function(opts)
    state.parametrized_test_num = opts.args
end, { nargs = 1 })

---@return table
---@param context task.lang.Context
function M.build_run_test_cmd(context)
    return build_junit_tests_cmd(context)
end

local test_selector_resolver = {
    [task.test_type.ALL_TESTS] = function()
        local module_path = java_util.get_buffer_project_path()
        local test_classes = module_path .. "/target/test-classes"
        return "--scan-class-path=" .. test_classes
    end,
    [task.test_type.FILE_TESTS] = function()
        local current_class_fqn = java_ts.get_class_name()
        if current_class_fqn == nil then
            vim.notify("Wrong junit selector context to: FILE_TESTS", vim.log.levels.WARN)
            return nil
        end
        return "--select-class=" .. current_class_fqn
    end,
    [task.test_type.CURRENT_TEST] = function()
        local current_test_method_fqn = java_ts.get_full_method_with_params("#")
        if current_test_method_fqn == nil then
            vim.notify("Wrong junit selector context to: CURRENT_TEST", vim.log.levels.WARN)
            return nil
        end

        local module_path = java_util.get_buffer_project_path()
        local test_classes = module_path .. "/target/test-classes"
        current_test_method_fqn =
            javap_util.resolve_parametrized_method_signature(current_test_method_fqn, test_classes)
        -- vim.notify("signature: " .. current_test_method_fqn, vim.log.levels.WARN)
        return "--select-method=" .. current_test_method_fqn
    end,
    [task.test_type.CURRENT_PARAMETRIZED_NUM_TEST] = function()
        local current_test_method_fqn = java_ts.get_full_method_with_params("#")
        if current_test_method_fqn == nil then
            vim.notify("Wrong junit selector context to: CURRENT_TEST", vim.log.levels.WARN)
            return nil
        end
        state.parametrized_test_num = nio_util.input("Test Number")
        local module_path = java_util.get_buffer_project_path()
        local test_classes = module_path .. "/target/test-classes"
        current_test_method_fqn =
            javap_util.resolve_parametrized_method_signature(current_test_method_fqn, test_classes)
        -- vim.notify("signature: " .. current_test_method_fqn, vim.log.levels.WARN)
        return "--select-iteration=method:" .. current_test_method_fqn .. "[" .. state.parametrized_test_num .. "]"
    end,
}

---@param context task.lang.Context
---@return table
function build_junit_tests_cmd(context)
    local type = context.test_type
    local is_debug = context.is_debug

    local classpath = require("utils.java.jdtls-classpath-util").get_classpath_for_main_method()

    local module_path = java_util.get_buffer_project_path()
    local current_report_dir = module_path .. setting.report_dir

    local test_selector = test_selector_resolver[type]()
    if test_selector == nil then
        return { "echo", "Wrong test selector context!" }
    end

    local debug_param = is_debug and "-agentlib:jdwp=transport=dt_socket,server=y,suspend=y,address=5005" or nil

    return vim.iter({
        java_util.java_bin,
        debug_param,
        setting.jvm_args,
        "-jar",
        setting.junit_jar,
        "execute",
        "--classpath=" .. classpath,
        "--reports-dir=" .. current_report_dir,
        "--fail-if-no-tests",
        "--disable-banner",
        -- "--details=testfeed",
        "--config=junit.platform.output.capture.stdout=true",
        "--config=junit.platform.output.capture.stderr=true",
        "--include-engine=junit-jupiter",
        "--include-classname=(^.*Tests?$|^.*IT$|^.*Spec$)",
        test_selector,
    })
        :flatten()
        :filter(function(v)
            return v ~= nil
        end)
        :totable()
end

-- print(vim.iter({
--     1,
--     2,
--     3,
--     { 4, 5, 6 },
--     7,
--     { 8, 9 },
-- })
--     :flatten()
--     :totable())

return M
