-- C# runner: app run/debug (dotnet run / netcoredbg). Test run/debug is delegated
-- to modules.cs.dotnet-test (see lua/modules/cs/CS_TESTS.md).

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

    dap.run(config)
end

function M.dap_launch_rerun()
    M.dap_launch()
end

--------------------------------------------------------------------------------
-- Tests (delegated to modules.cs.dotnet-test).
-- NOTE: intentionally no dap_launch_test, so debug runs go through the overseer
-- DEBUG_TESTS task + dap_ctrl_component (VSTEST_HOST_DEBUG + netcoredbg attach),
-- giving <leader>tl rerun-in-debug and <leader>tD toggle parity with Java.

---@param context task.lang.Context
---@return boolean ok, string|nil err
function M.prepare_test_context(context)
    return require("modules.cs.dotnet-test").prepare_test_context(context)
end

---@param context task.lang.Context
---@return task.lang.test.TestCmd
function M.build_run_test_cmd(context)
    return require("modules.cs.dotnet-test").build_run_test_cmd(context)
end

---@return string
function M.get_test_report_dir()
    return require("modules.cs.dotnet-test").get_test_report_dir()
end

--- Output-driven DAP attach for the overseer debug-task flow. Tests run under
--- VSTEST_HOST_DEBUG=1, so the testhost prints "Process Id: N, Name: testhost" and
--- waits; debug/dap_ctrl_component scans output and calls this once it appears.
---@type task.lang.DapOutputAttacher
M.dap_output_attacher = {
    name = "netcoredbg-testhost",
    -- VSTEST_HOST_DEBUG banner: "Process Id: <pid>, Name: <dotnet|testhost>".
    -- The process name varies by SDK version, so match any name, anchored by the
    -- unique "Process Id: N, Name:" shape to avoid false positives.
    match = function(line)
        return line:match("Process Id:%s*(%d+),%s*Name:")
    end,
    attach = function(pid)
        vim.notify("Attaching netcoredbg to testhost pid: " .. pid)
        require("modules.cs.dotnet-test").dap_attach_testhost(tonumber(pid))
    end,
}

return M
