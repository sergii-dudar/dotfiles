local M = {}

---@return table
function M.build_run_cmd()
    local file = vim.fn.expand("%:p")
    return { "go", "run", file }
end

function M.dap_launch()
    require("dap").run({
        type = "go",
        name = "Debug",
        request = "launch",
        program = "${file}",
    })
end

function M.dap_launch_rerun()
    M.dap_launch()
end

function M.dap_launch_test(context)
    return require("modules.go.go-test").dap_launch_test(context)
end

---@param context task.lang.Context
---@return boolean ok, string|nil err
function M.prepare_test_context(context)
    return require("modules.go.go-test").prepare_test_context(context)
end

---@param context task.lang.Context
---@return task.lang.test.TestCmd
function M.build_run_test_cmd(context)
    return require("modules.go.go-test").build_run_test_cmd(context)
end

---@return string|nil
function M.get_test_report_dir()
    return require("modules.go.go-test").get_test_report_dir()
end

return M
