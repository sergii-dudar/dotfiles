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
    local dir = vim.fn.expand("%:p:h")
    local fileName = vim.fn.expand("%:t")
    local fileNameWithoutExt = vim.fn.expand("%:t:r")

    -- Set makeprg for this buffer
    local compile_cmd = string.format(
        "cd %s && gcc -g -std=c17 -Wno-format %s -o /tmp/%s",
        dir, fileName, fileNameWithoutExt
    )
    vim.bo.makeprg = compile_cmd

    -- Compile synchronously
    vim.cmd("silent make")

    -- Check if compilation succeeded by checking quickfix list
    local qflist = vim.fn.getqflist()
    if #qflist > 0 then
        vim.cmd("copen")  -- Show errors
        print("Compilation failed. Fix errors before debugging.")
        return
    end

    print("Compilation successful. Starting debugger...")

    -- Launch DAP if compilation succeeded
    require("dap").run({
        type = "codelldb",
        request = "launch",
        name = "Launch file",
        program = "/tmp/" .. fileNameWithoutExt,
        cwd = "${workspaceFolder}",
    })
    vim.cmd("Neotree close")
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
