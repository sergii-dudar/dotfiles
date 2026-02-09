local overseer_task_util = require("plugins.overseer.overseer-task-util")

local M = {}

function M.build_compile_cmd()
    local dir = vim.fn.expand("%:p:h")
    return {
        "sh",
        "-c",
        "cd " .. dir .. " && cargo build",
    }
end

---@return table
function M.build_run_cmd()
    local dir = vim.fn.expand("%:p:h")
    return {
        "sh",
        "-c",
        "cd " .. dir .. " && cargo -q run",
    }
end

-- ---@return table
-- function M.build_debug_cmd()
-- end

-- function M.dap_attach_to_remote(port)
-- end

function M.dap_launch()
    overseer_task_util.run_compile(function()
        require("dap").run({
            name = "LLDB: Launch",
            type = "codelldb",
            request = "launch",
            -- TODO:
            program = "/home/serhii/serhii.home/git/java_sandbox/rust-sandbox/tutorials/01basics/04loop/target/debug/learn",
            -- program = function()
            --     return vim.fn.input("Path to executable: ", vim.fn.getcwd() .. "/", "file")
            -- end,
            cwd = "${workspaceFolder}",
            stopOnEntry = false,
            args = {},
            console = "integratedTerminal",
        })
        vim.cmd("Neotree close")
    end)
end

function M.dap_launch_rerun()
    M.dap_launch()
end

return M
