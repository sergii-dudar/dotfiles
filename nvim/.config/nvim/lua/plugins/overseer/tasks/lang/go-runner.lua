local M = {}

---@return table
function M.build_run_cmd()
    local file = vim.fn.expand("%:p")
    return { "go", "run", file }
end

-- ---@return table
-- function M.build_debug_cmd()
-- end

-- function M.dap_attach_to_remote(port)
-- end

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

return M
