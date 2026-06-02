-- Lua / busted overseer runner: wires the modules.lua.busted-test module into
-- the overseer test pipeline. Mirrors rust-runner.lua / go-runner.lua.
--
-- Owns TEST commands (<leader>tt, <leader>td, <leader>tf, <leader>ta, ...).
-- Also provides <leader>rd (debug current .lua file via
-- local-lua-debugger-vscode). Plain <leader>dl in-nvim debugging
-- continues to use osv as before.

local M = {}

---@return table
function M.build_run_cmd()
    -- local file = vim.fn.expand("%:p")
    -- return { "nvim", "-l", file }
    local dir = vim.fn.expand("%:p:h")
    local fileName = vim.fn.expand("%:t")
    return { "lua", dir .. "/" .. fileName }
end

---@param context task.lang.Context
function M.dap_launch_test(context)
    return require("modules.lua.busted-test").dap_launch_test(context)
end

--- <leader>rd entry point — debug the current .lua file via
--- local-lua-debugger-vscode (Mason). Does not require busted.
function M.dap_launch()
    return require("modules.lua.busted-test").dap_launch_current_file()
end

---@param context task.lang.Context
---@return boolean ok, string|nil err
function M.prepare_test_context(context)
    return require("modules.lua.busted-test").prepare_test_context(context)
end

---@param context task.lang.Context
---@return task.lang.test.TestCmd
function M.build_run_test_cmd(context)
    return require("modules.lua.busted-test").build_run_test_cmd(context)
end

---@return string|nil
function M.get_test_report_dir()
    return require("modules.lua.busted-test").get_test_report_dir()
end

return M