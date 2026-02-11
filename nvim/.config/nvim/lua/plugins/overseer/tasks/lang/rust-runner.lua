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

---@return table|nil
local function get_rust_binary_info()
    local dir = vim.fn.expand("%:p:h")
    local cargo_metadata_cmd = string.format("cd %s && cargo metadata --format-version 1 --no-deps 2>/dev/null", dir)
    local handle = io.popen(cargo_metadata_cmd)
    local metadata_json = handle:read("*a")
    handle:close()

    local metadata = vim.fn.json_decode(metadata_json)
    if not metadata or not metadata.workspace_root then
        vim.notify("Failed to get Cargo metadata", vim.log.levels.ERROR)
        return nil
    end

    local workspace_root = metadata.workspace_root

    -- Find the first binary target
    local binary_name = nil
    for _, package in ipairs(metadata.packages) do
        for _, target in ipairs(package.targets) do
            if vim.tbl_contains(target.kind, "bin") then
                binary_name = target.name
                break
            end
        end
        if binary_name then
            break
        end
    end

    if not binary_name then
        vim.notify("No binary target found in Cargo project", vim.log.levels.ERROR)
        return nil
    end

    local program = workspace_root .. "/target/debug/" .. binary_name

    return {
        program = program,
        workspace_root = workspace_root,
    }
end

function M.dap_launch()
    overseer_task_util.run_compile(function()
        local binary_info = get_rust_binary_info()
        if not binary_info then
            return
        end

        require("dap").run({
            name = "LLDB: Launch",
            type = "codelldb",
            request = "launch",
            program = binary_info.program,
            cwd = binary_info.workspace_root,
            -- cwd = "${workspaceFolder}",
            stopOnEntry = false,
            args = {},
            console = "integratedTerminal",
        })
    end)
end

function M.dap_launch_rerun()
    M.dap_launch()
end

return M
