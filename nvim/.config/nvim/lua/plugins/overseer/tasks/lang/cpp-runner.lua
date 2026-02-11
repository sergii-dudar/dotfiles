local overseer_task_util = require("plugins.overseer.overseer-task-util")

local M = {}

function M.build_compile_cmd()
    local fileNameWithoutExt = vim.fn.expand("%:t:r")
    local fileDir = vim.fn.expand("%:p:h")
    return {
        "sh",
        "-c",
        "g++ -std=c++23 -g " .. fileDir .. "/" .. fileNameWithoutExt .. "*.cpp -o /tmp/" .. fileNameWithoutExt,
    }
end

---@return table
function M.build_run_cmd()
    local fileNameWithoutExt = vim.fn.expand("%:t:r")
    local fileDir = vim.fn.expand("%:p:h")
    return {
        "sh",
        "-c",
        "g++ -std=c++23 -g "
            .. fileDir
            .. "/"
            .. fileNameWithoutExt
            .. "*.cpp -o /tmp/"
            .. fileNameWithoutExt
            .. " && /tmp/"
            .. fileNameWithoutExt
            .. " && rm /tmp/"
            .. fileNameWithoutExt,
    }
end

-- ---@return table
-- function M.build_debug_cmd()
-- end

-- function M.dap_attach_to_remote(port)
-- end

function M.dap_launch()
    overseer_task_util.run_compile(function()
        local fileNameWithoutExt = vim.fn.expand("%:t:r")
        require("dap").run({
            type = "codelldb",
            request = "launch",
            name = "Launch file",
            program = "/tmp/" .. fileNameWithoutExt,
            cwd = "${workspaceFolder}",
        })
    end)
end

function M.dap_launch_rerun()
    M.dap_launch()
end

return M
