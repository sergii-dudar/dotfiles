local M = {}

---@return table
function M.build_run_cmd()
    local file = vim.fn.expand("%:p")
    return { "deno", "run", file }
end

-- ---@return table
-- function M.build_debug_cmd()
-- end

-- function M.dap_attach_to_remote(port)
-- end

function M.dap_launch()
    require("dap").run({
        type = "pwa-node",
        request = "launch",
        name = "Launch file",
        program = "${file}",
        cwd = "${workspaceFolder}",
        sourceMaps = true,
        -- runtimeExecutable = vim.fn.executable("tsx") == 1 and "tsx" or "ts-node",
        skipFiles = {
            "<node_internals>/**",
            "node_modules/**",
        },
        resolveSourceMapLocations = {
            "${workspaceFolder}/**",
            "!**/node_modules/**",
        },
    })
    vim.cmd("Neotree close")
end

function M.dap_launch_rerun()
    M.dap_launch()
end

return M
