local M = {}

---@return table
function M.build_run_cmd()
    local dir = vim.fn.expand("%:p:h")
    local fileName = vim.fn.expand("%:t")
    local fileNameWithoutExt = vim.fn.expand("%:t:r")
    return {
        "sh",
        "-c",
        "cd "
            .. dir
            .. " && gcc -g -std=c17 -Wno-format "
            .. fileName
            .. " -o /tmp/"
            .. fileNameWithoutExt
            .. " && /tmp/"
            .. fileNameWithoutExt,
        -- .. " && rm /tmp/"
        -- .. fileNameWithoutExt,
    }
end

-- ---@return table
-- function M.build_debug_cmd()
-- end

-- function M.dap_attach_to_remote(port)
-- end

function M.dap_launch()
    local overseer = require("overseer")
    local fileNameWithoutExt = vim.fn.expand("%:t:r")
    local executable_path = "/tmp/" .. fileNameWithoutExt

    -- Create a compilation task
    local task = overseer.new_task({
        name = "Compile for debug",
        cmd = M.build_run_cmd(),
        components = {
            { "on_output_quickfix", set_diagnostics = true },
            "on_result_diagnostics",
            "on_exit_set_status",
        },
    })

    -- Subscribe to task completion
    task:subscribe("on_complete", function(t, status)
        if status == overseer.STATUS.SUCCESS then
            print("Compilation successful. Starting debugger...")
            require("dap").run({
                type = "codelldb",
                request = "launch",
                name = "Launch file",
                program = executable_path,
                cwd = "${workspaceFolder}",
            })
            vim.cmd("Neotree close")
        else
            print("Compilation failed. Fix errors before debugging.")
            vim.cmd("copen") -- Open quickfix to show errors
        end
    end)

    -- Start compilation
    task:start()
end

function M.dap_launch_rerun()
    -- For rerun, we can directly launch without recompiling
    -- since the binary should already exist from the first run
    local fileNameWithoutExt = vim.fn.expand("%:t:r")
    require("dap").run({
        type = "codelldb",
        request = "launch",
        name = "Launch file",
        program = "/tmp/" .. fileNameWithoutExt,
        cwd = "${workspaceFolder}",
    })
    vim.cmd("Neotree close")
end

return M
