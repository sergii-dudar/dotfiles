local M = {}

---@return table
function M.build_run_cmd()
    local dir = vim.fn.expand("%:p:h")
    return {
        "sh",
        "-c",
        "cd " .. dir .. " && dotnet run",
    }
end

-- ---@return table
-- function M.build_debug_cmd()
-- end

-- function M.dap_attach_to_remote(port)
-- end

-- Helper function to find .csproj and determine DLL path
local function get_dll_path()
    -- Start from current file's directory
    local current_file = vim.fn.expand("%:p")
    local current_dir = vim.fn.fnamemodify(current_file, ":h")

    -- Search upwards for .csproj file
    local function find_csproj(dir)
        local csproj = vim.fn.glob(dir .. "/*.csproj")
        if csproj ~= "" then
            return csproj, dir
        end

        local parent = vim.fn.fnamemodify(dir, ":h")
        -- Stop if we've reached root or can't go higher
        if parent == dir or parent == "" then
            return nil, nil
        end

        return find_csproj(parent)
    end

    local csproj, project_dir = find_csproj(current_dir)

    if csproj ~= "" and csproj ~= nil then
        local project_name = vim.fn.fnamemodify(csproj, ":t:r")

        -- Try to detect target framework from bin directory
        local bin_debug = project_dir .. "/bin/Debug"
        local net_dirs = vim.fn.glob(bin_debug .. "/net*", false, true)

        if #net_dirs > 0 then
            -- Use the first (or most recent) framework found
            local framework = vim.fn.fnamemodify(net_dirs[1], ":t")
            return string.format("%s/bin/Debug/%s/%s.dll", project_dir, framework, project_name)
        end
    end

    -- Fallback to manual input
    local fallback_dir = project_dir or vim.fn.getcwd()
    return vim.fn.input("Path to dll: ", fallback_dir .. "/bin/Debug/", "file")
end

function M.dap_launch()
    -- TODO: is not ready yet, looks debug .net 10 is not well suported yet.
    local dap = require("dap")

    -- Ensure netcoredbg adapter is configured
    if not dap.adapters.netcoredbg then
        dap.adapters.netcoredbg = {
            type = "executable",
            command = vim.fn.exepath("netcoredbg"),
            args = { "--interpreter=vscode" },
            options = {
                detached = false,
            },
        }
    end

    local current_file = vim.fn.expand("%:p")
    local current_dir = vim.fn.fnamemodify(current_file, ":h")

    -- Find project directory by searching for .csproj
    local function find_project_dir(dir)
        local csproj = vim.fn.glob(dir .. "/*.csproj")
        if csproj ~= "" then
            return dir
        end
        local parent = vim.fn.fnamemodify(dir, ":h")
        if parent == dir or parent == "" then
            return vim.fn.getcwd()
        end
        return find_project_dir(parent)
    end

    local project_dir = find_project_dir(current_dir)
    local dll_path = get_dll_path()

    local config = {
        type = "netcoredbg",
        name = "Launch .NET Application",
        request = "launch",
        program = dll_path,
        cwd = project_dir,
        stopAtEntry = false,
        console = "integratedTerminal",
        env = function()
            local vars = {}
            for k, v in pairs(vim.fn.environ()) do
                vars[k] = v
            end
            return vars
        end,
    }

    dd({ config = config, adapter = dap.adapters.netcoredbg })
    dap.run(config)
    vim.cmd("Neotree close")
end

function M.dap_launch_rerun()
    M.dap_launch()
end

return M