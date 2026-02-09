local M = {}

---@return table
function M.build_run_cmd()
    local fileNameWithoutExt = vim.fn.expand("%:t:r")
    local fileDir = vim.fn.expand("%:p:h")
    return {
        "sh",
        "-c",
        "g++ -g -std=c++23 "
            .. fileDir
            .. "/"
            .. fileNameWithoutExt
            .. "*.cpp -o /tmp/"
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
    require("dap").run({
        type = "codelldb",
        request = "launch",
        name = "Launch file",
        program = function()
            -- return vim.fn.input("Path to executable: ", vim.fn.getcwd() .. "/", "file")
            local fileNameWithoutExt = vim.fn.expand("%:t:r")
            return "/tmp/" .. fileNameWithoutExt
        end,
        cwd = "${workspaceFolder}",
    })
    vim.cmd("Neotree close")
end

function M.dap_launch_rerun()
    M.dap_launch()
end

return M
