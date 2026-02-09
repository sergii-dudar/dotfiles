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
    vim.cmd("Neotree close")
end

function M.dap_launch_rerun()
    M.dap_launch()
end

return M
