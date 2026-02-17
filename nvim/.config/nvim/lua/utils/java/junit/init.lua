local java_util = require("utils.java.java-common")

local M = {}

local setting = {
    junit_jar = vim.fn.glob("$HOME/tools/java-extensions/junit/junit-platform-console-standalone.jar"),
    jvm_args = {
        string.format("-javaagent:%s/tools/java-extensions/jmockit/jmockit.jar", os.getenv("HOME")),
    },
    report_dir = "/target/junit-report",
}

---@return table
function M.build_run_all_tests_cmd(is_debug)
    local classpath = require("utils.java.jdtls-classpath-util").get_classpath_for_main_method()
    local module_path = java_util.get_buffer_project_path()
    local test_classes = module_path .. "/target/test-classes"
    local current_report_dir = module_path .. setting.report_dir
    return {
        java_util.java_bin,
        "-jar",
        setting.junit_jar,
        "execute",
        "--classpath=" .. classpath,
        "--reports-dir=" .. current_report_dir,
        "--fail-if-no-tests",
        "--disable-banner",
        "--details=testfeed",
        "--config=junit.platform.output.capture.stdout=true",
        "--config=junit.platform.output.capture.stderr=true",
        "--include-engine junit-jupiter",
        -- "--include-classname=(^.*Tests?$|^.*IT$|^.*Spec$)",
        "--scan-class-path=" .. test_classes,
    }
end

---@return table
function M.build_run_file_tests_cmd(is_debug)
    vim.notify("build_run_file_tests_cmd")
    return { "echo", "test run: build_run_file_tests_cmd" }
    -- TODO:
end

---@return table
function M.build_run_test_cmd(is_debug)
    vim.notify("build_run_test_cmd")
    return { "echo", "test run: build_run_test_cmd" }
    -- TODO:
end

---@return table
function M.build_run_parametrized_num_test_cmd(is_debug)
    vim.notify("build_run_parametrized_num_test_cmd")
    return { "echo", "test run: build_run_parametrized_num_test_cmd" }
    -- TODO:
end

local read_xml = function(filepath)
    local xml = require("lib.xml")
    local file = require("lib.file")
    local content = file.read_file(
        "/Users/iuada144/serhii.home/work/git.work/ua-payments-payment-prevalidation/payment-prevalidation/pom.xml"
    )
    -- print(content)
    -- print(xml.parse(content))
    -- print(xml.parse(content).project.artifactId)
end

return M
