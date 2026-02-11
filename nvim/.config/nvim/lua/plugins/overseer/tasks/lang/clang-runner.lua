local overseer_task_util = require("plugins.overseer.overseer-task-util")

local M = {}

function M.build_compile_cmd()
    local dir = vim.fn.expand("%:p:h")
    local fileName = vim.fn.expand("%:t")
    local fileNameWithoutExt = vim.fn.expand("%:t:r")
    return {
        "sh",
        "-c",
        "cd " .. dir .. " && gcc -g -std=c17 -Wno-format " .. fileName .. " -o /tmp/" .. fileNameWithoutExt,
    }
end

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

--[[ ---@return boolean - true when success
function make_compile()
    local dir = vim.fn.expand("%:p:h")
    local fileName = vim.fn.expand("%:t")
    local fileNameWithoutExt = vim.fn.expand("%:t:r")

    -- Set makeprg for this buffer
    local compile_cmd =
        string.format("cd %s && gcc -g -std=c17 -Wno-format %s -o /tmp/%s", dir, fileName, fileNameWithoutExt)
    vim.bo.makeprg = compile_cmd

    -- Compile synchronously
    vim.cmd("silent make")

    -- Check if compilation succeeded by checking quickfix list
    local qflist = vim.fn.getqflist()
    if #qflist > 0 then
        vim.cmd("Trouble qflist open")
        -- vim.cmd("Trouble diagnostics open")
        -- vim.cmd("copen") -- Show errors
        vim.notify("Compilation failed. Fix errors before debugging.", vim.log.levels.WARN)
        return false
    end

    vim.notify("Compilation successful. Starting debugger...")
    return true
end

function M.dap_launch()
    if not make_compile() then
        return
    end

    local fileNameWithoutExt = vim.fn.expand("%:t:r")
    require("dap").run({
        type = "codelldb",
        request = "launch",
        name = "Launch file",
        program = "/tmp/" .. fileNameWithoutExt,
        cwd = "${workspaceFolder}",
    })
end ]]

return M
