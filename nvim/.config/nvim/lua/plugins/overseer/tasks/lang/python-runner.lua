local M = {}

---@return table
function M.build_run_cmd()
    local file = vim.fn.expand("%:p")
    return { "python3.14", file }
end

-- ---@return table
-- function M.build_debug_cmd()
-- end

-- function M.dap_attach_to_remote(port)
-- end

function M.dap_launch()
    require("dap").run({
        type = "python",
        request = "launch",
        name = "Run File",
        -- `program` is what you'd use in `python <program>` in a shell
        -- If you need to run the equivalent of `python -m <module>`, replace
        -- `program = '${file}` entry with `module = "modulename"
        program = "${file}",
        console = "integratedTerminal",
    })
end

function M.dap_launch_rerun()
    M.dap_launch()
end

--------------------------------------------------------------------------------
-- pytest test integration (delegates to modules.python.pytest-test).
-- pytest discovers unittest.TestCase natively, so unittest projects are
-- supported via the same path without extra deps.

---@param context task.lang.Context
---@return boolean ok, string|nil err
function M.prepare_test_context(context)
    return require("modules.python.pytest-test").prepare_test_context(context)
end

---@param context task.lang.Context
---@return task.lang.test.TestCmd
function M.build_run_test_cmd(context)
    return require("modules.python.pytest-test").build_run_test_cmd(context)
end

---@param context task.lang.Context
function M.dap_launch_test(context)
    return require("modules.python.pytest-test").dap_launch_test(context)
end

---@return string|nil
function M.get_test_report_dir()
    return require("modules.python.pytest-test").get_test_report_dir()
end

return M
