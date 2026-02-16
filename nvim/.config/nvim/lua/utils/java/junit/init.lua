local M = {}

---@return table
function M.build_run_all_tests_cmd(is_debug)
    vim.notify("build_run_all_tests_cmd")
    return { "echo", "test run: build_run_all_tests_cmd" }
    -- TODO:
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
