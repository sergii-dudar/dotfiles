local java_util = require("utils.java.java-common")
local java_ts = require("utils.java.java-ts-util")
local javap_util = require("utils.java.javap-util")
local jdtls_util = require("utils.java.jdtls-util")
local string_util = require("utils.string-util")
local nio_util = require("utils.nio-util")
-- local common_util = require("utils.common-util")
local jdtls_classpath = require("utils.java.jdtls-classpath-util")
local constants = require("utils.constants")

local M = {}

local home = os.getenv("HOME")

--[[ local mockito_core_version = "5.20.0"
local mockito_core_jar = string.format(
    "%s/.m2/repository/org/mockito/mockito-core/%s/mockito-core-%s.jar",
    home,
    mockito_core_version,
    mockito_core_version
)
if common_util.is_file_exists(mockito_core_jar) then
    table.insert(setting.jvm_args, "-javaagent:" .. mockito_core_jar)
end ]]

-- Resolve latest byte-buddy-agent version from local maven cache
local byte_buddy_agent_dir = home .. "/.m2/repository/net/bytebuddy/byte-buddy-agent"
local byte_buddy_agent_jar = vim.fn.glob(byte_buddy_agent_dir .. "/*/byte-buddy-agent-*.jar", false, true)
-- glob returns sorted, last entry is latest version
byte_buddy_agent_jar = byte_buddy_agent_jar[#byte_buddy_agent_jar]

local setting = {
    junit_jar = vim.fn.glob("$HOME/tools/java-extensions/junit/junit-platform-console-standalone.jar"),
    jvm_args = {
        "--enable-native-access=ALL-UNNAMED",
        "-Dspring.output.ansi.enabled=NEVER",
        string.format("-javaagent:%s/tools/java-extensions/jmockit/jmockit.jar", home),
    },
    report_dir = constants.java.junit_report_dir,
}
if byte_buddy_agent_jar then
    table.insert(setting.jvm_args, "-javaagent:" .. byte_buddy_agent_jar)
end

---@param module_path string
---@return boolean
local function has_test_classes(module_path)
    local test_classes_dir = module_path .. "/target/test-classes"
    local test_sources_dir = module_path .. "/src/test"
    if vim.fn.isdirectory(test_sources_dir) ~= 1 or vim.fn.isdirectory(test_classes_dir) ~= 1 then
        return false
    end
    local class_files = vim.fn.glob(test_classes_dir .. "/**/*.class", false, true)
    for _, file in ipairs(class_files) do
        local basename = vim.fn.fnamemodify(file, ":t:r")
        if basename:match("Tests?$") or basename:match("IT$") or basename:match("Spec$") then
            return true
        end
    end
    return false
end

local state = {
    parametrized_test_num = 0,
}

vim.api.nvim_create_user_command("ParamTestNum", function(opts)
    state.parametrized_test_num = opts.args
end, { nargs = 1 })

---@param context task.lang.Context
---@return task.lang.test.TestCmd
function M.build_run_test_cmd(context)
    return build_junit_tests_cmd(context)
end

local test_selector_resolver = {
    [task.test_type.ALL_TESTS] = function()
        local module_path = java_util.get_buffer_project_path()
        local test_classes = module_path .. "/target/test-classes"
        return "--scan-class-path=" .. test_classes
    end,
    [task.test_type.ALL_DIR_TESTS] = function()
        local test_package = java_ts.get_class_package()
        if test_package == nil then
            vim.notify("Wrong junit selector context to: FILE_TESTS", vim.log.levels.WARN)
            return nil
        end
        return "--select-package=" .. test_package
    end,
    [task.test_type.FILE_TESTS] = function()
        local current_class = java_ts.get_class_name_with_abstract()
        if current_class == nil or vim.tbl_isempty(current_class) then
            current_class = java_ts.get_root_class_with_abstract()
            if current_class == nil then
                vim.notify("Wrong junit selector context to: FILE_TESTS", vim.log.levels.WARN)
                return nil
            end
        end
        local current_class_fqn = current_class.fqn
        if not current_class.is_abstract then
            return "--select-class=" .. current_class_fqn
        end

        local impls = jdtls_util.jdt_find_implementations_nio(current_class_fqn)
        if not vim.tbl_isempty(impls) then
            if #impls == 1 then
                return "--select-class=" .. impls[1]
            end
            local picked_impl = nio_util.select(impls, "Select implementation to run")
            return "--select-class=" .. picked_impl
        end
        vim.notify("No any implementations found for: " .. current_class_fqn)
        return "--select-class=" .. current_class_fqn
    end,
    [task.test_type.CURRENT_TEST] = function()
        local current_test_method = java_ts.get_full_method_with_params_and_abstract("#")
        if current_test_method == nil then
            vim.notify("Wrong junit selector context to: CURRENT_TEST", vim.log.levels.WARN)
            return nil
        end

        local module_path = java_util.get_buffer_project_path()
        local test_classes = module_path .. "/target/test-classes"
        local current_test_method_fsignature =
            javap_util.resolve_parametrized_method_signature(current_test_method.fsignature, test_classes)
        -- vim.notify("signature: " .. current_test_method_fqn, vim.log.levels.WARN)

        if not current_test_method.is_abstract then
            return "--select-method=" .. current_test_method_fsignature
        end

        local current_test_method_parts = string_util.split(current_test_method_fsignature, "#")
        local current_class_fqn = current_test_method_parts[1]
        local method_signature = current_test_method_parts[2]

        local impls = jdtls_util.jdt_find_implementations_nio(current_class_fqn)
        if not vim.tbl_isempty(impls) then
            if #impls == 1 then
                return "--select-method=" .. impls[1] .. "#" .. method_signature
            end
            local picked_impl = nio_util.select(impls, "Select implementation to run")
            return "--select-method=" .. picked_impl .. "#" .. method_signature
        end
        vim.notify("No any implementations found for: " .. current_class_fqn)
        return "--select-method=" .. current_test_method_fsignature
    end,
    [task.test_type.CURRENT_PARAMETRIZED_NUM_TEST] = function()
        local current_test_method_fqn = java_ts.get_full_method_with_params("#")
        if current_test_method_fqn == nil then
            vim.notify("Wrong junit selector context to: CURRENT_TEST", vim.log.levels.WARN)
            return nil
        end
        state.parametrized_test_num = tonumber(nio_util.input("Test Number")) - 1
        local module_path = java_util.get_buffer_project_path()
        local test_classes = module_path .. "/target/test-classes"
        current_test_method_fqn =
            javap_util.resolve_parametrized_method_signature(current_test_method_fqn, test_classes)
        -- vim.notify("signature: " .. current_test_method_fqn, vim.log.levels.WARN)
        return "--select-iteration=method:" .. current_test_method_fqn .. "[" .. state.parametrized_test_num .. "]"
    end,
}

---@param opts { classpath: string, report_dir: string, test_selector: string, is_debug?: boolean }
---@return string[]
local function build_single_module_cmd(opts)
    local debug_param = opts.is_debug and "-agentlib:jdwp=transport=dt_socket,server=y,suspend=y,address=5005" or nil

    return vim.iter({
        java_util.java_bin,
        debug_param,
        setting.jvm_args,
        "-jar",
        setting.junit_jar,
        "execute",
        "--classpath=" .. opts.classpath,
        "--reports-dir=" .. opts.report_dir,
        "--fail-if-no-tests",
        "--disable-banner",
        -- "--single-color",
        -- "--disable-ansi-colors",
        "--details=testfeed", -- tree (default) broke logs hightlight roles
        "--config=junit.platform.output.capture.stdout=true",
        "--config=junit.platform.output.capture.stderr=true",
        "--include-engine=junit-jupiter",
        "--include-classname=(^.*Tests?$|^.*IT$|^.*Spec$)",
        opts.test_selector,
    })
        :flatten()
        :filter(function(v)
            return v ~= nil
        end)
        :totable()
end

local function cmd_to_string(cmd_table)
    local parts = {}
    for _, arg in ipairs(cmd_table) do
        if arg:find("[%s%(%)%$%|%*%^&;><\"'\\#?!{}%[%]~`]") then
            table.insert(parts, "'" .. arg:gsub("'", "'\\''") .. "'")
        else
            table.insert(parts, arg)
        end
    end
    return table.concat(parts, " ")
end

--- Build a chained shell command running tests for each module
---@param modules { uri: string, path: string, name: string }[]
---@return task.lang.test.TestCmd
local function build_multi_module_cmd(modules)
    local cmds = {}
    local report_dirs = {}

    for _, mod in ipairs(modules) do
        local test_classes = mod.path .. "/target/test-classes"
        if has_test_classes(mod.path) then
            local classpath = jdtls_classpath.get_classpath_for_module_uri(mod.uri)
            if classpath then
                local report_dir = mod.path .. setting.report_dir
                local cmd_table = build_single_module_cmd({
                    classpath = classpath,
                    report_dir = report_dir,
                    test_selector = "--scan-class-path=" .. test_classes,
                })
                table.insert(cmds, cmd_to_string(cmd_table))
                table.insert(report_dirs, report_dir)
            end
        end
    end

    if #cmds == 0 then
        vim.notify("No modules with test-classes found", vim.log.levels.WARN)
        return nil
    end

    local isParallelRun = false -- just for testing for now
    local chained = "r=0"
    if isParallelRun then
        -- Run modules in parallel, collect exit codes
        for i, cmd_str in ipairs(cmds) do
            chained = chained .. "; " .. cmd_str .. " & p" .. i .. "=$!"
        end
        for i = 1, #cmds do
            chained = chained .. "; wait $p" .. i .. " || r=1"
        end
    else
        for _, cmd_str in ipairs(cmds) do
            chained = chained .. "; " .. cmd_str .. " || r=1"
        end
    end
    chained = chained .. "; exit $r"
    return { cmd = chained, report_dir = report_dirs }
end

---@param context task.lang.Context
---@return task.lang.test.TestCmd
function build_junit_tests_cmd(context)
    local type = context.test_type
    local is_debug = context.is_debug

    if type == task.test_type.ALL_MODULES_TESTS or type == task.test_type.SELECTED_MODULES_TESTS then
        local modules = jdtls_classpath.get_all_project_modules()
        if not modules then
            return { cmd = { "echo", "No modules found from jdtls" } }
        end
        modules = vim.tbl_filter(function(m)
            return has_test_classes(m.path)
        end, modules)
        if type == task.test_type.SELECTED_MODULES_TESTS then
            modules = nio_util.multi_select(modules, "Select modules to test")
            if not modules then
                return { cmd = { "echo", "No modules selected" } }
            end
        end
        return build_multi_module_cmd(modules) or { "echo", "No testable modules found" }
    end

    local classpath = require("utils.java.jdtls-classpath-util").get_classpath_for_main_method({ scope = "test" })
    local module_path = java_util.get_buffer_project_path()
    local current_report_dir = module_path .. setting.report_dir

    local test_selector = test_selector_resolver[type]()
    if test_selector == nil then
        return { cmd = { "echo", "Wrong test selector context!" } }
    end

    return {
        cmd = build_single_module_cmd({
            classpath = classpath,
            report_dir = current_report_dir,
            test_selector = test_selector,
            is_debug = is_debug,
        }),
        report_dir = java_util.get_buffer_project_path() .. setting.report_dir,
    }
end

return M
