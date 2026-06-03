local bash_path = vim.fn.has("mac") == 1 and "/opt/homebrew/bin/bash" or "/bin/bash"

local M = {}

---@return table
function M.build_run_cmd()
    local dir = vim.fn.expand("%:p:h")
    local fileName = vim.fn.expand("%:t")
    return { bash_path, dir .. "/" .. fileName }
end

-- ---@return table
-- function M.build_debug_cmd()
-- end

-- function M.dap_attach_to_remote(port)
-- end

function M.dap_launch()
    require("dap").run({
        type = "bashdb",
        request = "launch",
        name = "Launch file",
        showDebugOutput = true,
        pathBashdb = vim.fn.stdpath("data") .. "/mason/packages/bash-debug-adapter/extension/bashdb_dir/bashdb",
        pathBashdbLib = vim.fn.stdpath("data") .. "/mason/packages/bash-debug-adapter/extension/bashdb_dir",
        trace = true,
        file = "${file}",
        program = "${file}",
        cwd = "${workspaceFolder}",
        pathCat = "cat",
        pathBash = bash_path,
        pathMkfifo = "mkfifo",
        pathPkill = "pkill",
        args = {},
        env = {},
        terminalKind = "integrated",
    })
end

function M.dap_launch_rerun()
    M.dap_launch()
end

--------------------------------------------------------------------------------
-- bashunit test integration (delegates to modules.bash.bashunit-test).

---@param context task.lang.Context
---@return boolean ok, string|nil err
function M.prepare_test_context(context)
    return require("modules.bash.bashunit-test").prepare_test_context(context)
end

---@param context task.lang.Context
---@return task.lang.test.TestCmd
function M.build_run_test_cmd(context)
    return require("modules.bash.bashunit-test").build_run_test_cmd(context)
end

---@param context task.lang.Context
function M.dap_launch_test(context)
    return require("modules.bash.bashunit-test").dap_launch_test(context)
end

---@return string|nil
function M.get_test_report_dir()
    return require("modules.bash.bashunit-test").get_test_report_dir()
end

return M
