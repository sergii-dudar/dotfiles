-- C# test runner: builds `dotnet test` commands for each task.test_type.
--
-- See lua/modules/cs/CS_TESTS.md for the full design. Highlights:
--   * `dotnet test` with the built-in TRX logger (uniform across xunit / nunit /
--     mstest, no per-project NuGet packages required).
--   * Test discovery at cursor/file uses treesitter (modules.cs.cs-ts-util) to build
--     a `--filter "FullyQualifiedName~..."` expression.
--   * Debug tests has NO dap_launch_test on purpose: it runs as an overseer
--     DEBUG_TESTS task with VSTEST_HOST_DEBUG=1, and debug/dap_ctrl_component
--     attaches netcoredbg to the testhost when it prints its PID. This mirrors the
--     Java JDWP flow and gives <leader>tl rerun-in-debug + <leader>tD toggle parity.

local nio_util = require("utils.nio-util")
local cs_ts = require("modules.cs.cs-ts-util")

local M = {}

local state = {}

--- Walk upward from `start` looking for the first dir containing any of `patterns`.
---@param patterns string[]
---@param start string
---@return string|nil dir, string|nil match
local function find_upwards(patterns, start)
    local dir = start
    while dir and dir ~= "" do
        for _, p in ipairs(patterns) do
            local matches = vim.fn.glob(dir .. "/" .. p, false, true)
            if #matches > 0 then
                return dir, matches[1]
            end
        end
        local parent = vim.fn.fnamemodify(dir, ":h")
        if parent == dir then
            break
        end
        dir = parent
    end
    return nil, nil
end

---@return string
local function current_file_dir()
    local f = vim.fn.expand("%:p")
    if f == "" then
        return vim.fn.getcwd()
    end
    return vim.fn.fnamemodify(f, ":h")
end

--- Directory of the .csproj owning the current file (fallback: file dir).
---@return string
local function current_project_dir()
    local dir = find_upwards({ "*.csproj" }, current_file_dir())
    return dir or current_file_dir()
end

--- Run root for "all/modules" runs: nearest .sln dir, else current project dir.
---@return string
local function run_root_dir()
    local sln = find_upwards({ "*.sln" }, current_file_dir())
    return sln or current_project_dir()
end

--- List test projects (csproj referencing the VSTest SDK) under a root.
---@param root string
---@return { name: string, dir: string, csproj: string }[]
local function list_test_projects(root)
    local result = {}
    for _, csproj in ipairs(vim.fn.glob(root .. "/**/*.csproj", false, true)) do
        if not csproj:match("/bin/") and not csproj:match("/obj/") then
            local content = table.concat(vim.fn.readfile(csproj), "\n")
            if content:match("Microsoft%.NET%.Test%.Sdk") then
                table.insert(result, {
                    name = vim.fn.fnamemodify(csproj, ":t:r"),
                    dir = vim.fn.fnamemodify(csproj, ":h"),
                    csproj = csproj,
                })
            end
        end
    end
    return result
end

---@param dir string
---@return string
local function report_dir_for(dir)
    return dir .. "/TestResults/nvim"
end

--- Remove and recreate the report dir so stale TRX files don't pollute results.
---@param dir string
local function reset_report_dir(dir)
    vim.fn.delete(dir, "rf")
    vim.fn.mkdir(dir, "p")
end

--- Resolve the test selection for a non-toggle test type.
---@param context task.lang.Context
---@return { args: string[], cwd: string, report_dir: string, multi: table[]|nil }|nil
local function resolve_selection(context)
    local t = context.test_type

    if t == task.test_type.ALL_TESTS or t == task.test_type.ALL_MODULES_TESTS then
        local root = run_root_dir()
        return { args = {}, cwd = root, report_dir = report_dir_for(root) }
    end

    if t == task.test_type.ALL_DIR_TESTS then
        local proj = current_project_dir()
        return { args = {}, cwd = proj, report_dir = report_dir_for(proj) }
    end

    if t == task.test_type.SELECTED_MODULES_TESTS then
        local picked = context.selected_projects
        if not picked or #picked == 0 then
            vim.notify("No projects selected", vim.log.levels.WARN)
            return nil
        end
        local root = run_root_dir()
        return { args = {}, cwd = root, report_dir = report_dir_for(root), multi = picked }
    end

    if t == task.test_type.FILE_TESTS then
        local classes = cs_ts.test_classes(vim.api.nvim_get_current_buf())
        if #classes == 0 then
            vim.notify("No test classes found in file", vim.log.levels.WARN)
            return nil
        end
        local exprs = {}
        for _, cls in ipairs(classes) do
            table.insert(exprs, "FullyQualifiedName~" .. cls.fqn)
        end
        local proj = current_project_dir()
        return {
            args = { "--filter", table.concat(exprs, "|") },
            cwd = proj,
            report_dir = report_dir_for(proj),
        }
    end

    if t == task.test_type.CURRENT_TEST or t == task.test_type.CURRENT_PARAMETRIZED_NUM_TEST then
        -- For a parametrized method, FullyQualifiedName~Class.Method matches every
        -- data row, so <leader>tp runs the whole method (single-row filtering is
        -- framework-specific and unreliable across xunit/nunit/mstest).
        local bufnr = vim.api.nvim_get_current_buf()
        local row = vim.api.nvim_win_get_cursor(0)[1] - 1
        local tst = cs_ts.test_at_cursor(bufnr, row)
        if not tst then
            vim.notify("No test method at cursor", vim.log.levels.WARN)
            return nil
        end
        local proj = current_project_dir()
        return {
            args = { "--filter", "FullyQualifiedName~" .. tst.class_fqn .. "." .. tst.method },
            cwd = proj,
            report_dir = report_dir_for(proj),
        }
    end

    vim.notify("Unsupported C# test type: " .. tostring(t), vim.log.levels.WARN)
    return nil
end

--- Build the final overseer command from a resolved selection.
---@param sel { args: string[], cwd: string, report_dir: string, multi: table[]|nil }
---@param is_debug boolean
---@return task.lang.test.TestCmd|nil
local function build_cmd(sel, is_debug)
    reset_report_dir(sel.report_dir)

    if sel.multi then
        if is_debug then
            vim.notify("Debug is only supported for single test / file / project runs", vim.log.levels.WARN)
            return nil
        end
        local parts = { "r=0" }
        for _, proj in ipairs(sel.multi) do
            local body = string.format(
                "dotnet test %s --logger %s --results-directory %s --nologo",
                vim.fn.shellescape(proj.csproj),
                vim.fn.shellescape("trx;LogFileName=" .. proj.name .. ".trx"),
                vim.fn.shellescape(sel.report_dir)
            )
            table.insert(parts, body .. " || r=1")
        end
        table.insert(parts, "exit $r")
        return { cmd = { "sh", "-c", table.concat(parts, "; ") }, report_dir = sel.report_dir }
    end

    local escaped = {}
    for _, a in ipairs(sel.args) do
        table.insert(escaped, vim.fn.shellescape(a))
    end
    local body = string.format(
        "%sdotnet test %s --logger %s --results-directory %s --nologo",
        is_debug and "VSTEST_HOST_DEBUG=1 " or "",
        table.concat(escaped, " "),
        vim.fn.shellescape("trx;LogFileName=results.trx"),
        vim.fn.shellescape(sel.report_dir)
    )
    return { cmd = { "sh", "-c", body }, report_dir = sel.report_dir, cwd = sel.cwd }
end

--- Async pre-hook (still in nio context): prompt for module selection.
---@param context task.lang.Context
---@return boolean ok, string|nil err
function M.prepare_test_context(context)
    if context.is_debug then
        return true
    end
    if context.test_type == task.test_type.SELECTED_MODULES_TESTS then
        local root = run_root_dir()
        local projects = list_test_projects(root)
        if #projects == 0 then
            return false, "No test projects found under " .. root
        end
        local picked = nio_util.multi_select(projects, "Select test projects")
        if not picked or #picked == 0 then
            return false, "No projects selected"
        end
        context.selected_projects = picked
    end
    return true
end

---@param context task.lang.Context
---@return task.lang.test.TestCmd
function M.build_run_test_cmd(context)
    local is_debug = context.is_debug or false

    if context.test_type == task.test_type.TOGGLE_LAST_DEBUG then
        if not state.last then
            vim.notify("No previous C# test to toggle debug for", vim.log.levels.WARN)
            return { cmd = { "echo", "No previous C# test" } }
        end
        local cmd = build_cmd(state.last, is_debug)
        return cmd or { cmd = { "echo", "Could not build C# test command" } }
    end

    local sel = resolve_selection(context)
    if not sel then
        return { cmd = { "echo", "Could not build C# test command" } }
    end
    state.last = sel
    local cmd = build_cmd(sel, is_debug)
    return cmd or { cmd = { "echo", "Could not build C# test command" } }
end

---@return string
function M.get_test_report_dir()
    return report_dir_for(current_project_dir())
end

--- Attach netcoredbg to the VSTest testhost. Invoked by dap_ctrl_component when it
--- sees the "Process Id: N" banner emitted under VSTEST_HOST_DEBUG=1.
---@param pid integer
function M.dap_attach_testhost(pid)
    local dap = require("dap")
    if not dap.adapters.netcoredbg then
        dap.adapters.netcoredbg = {
            type = "executable",
            command = vim.fn.exepath("netcoredbg"),
            args = { "--interpreter=vscode" },
            options = { detached = false },
        }
    end
    dap.run({
        type = "netcoredbg",
        name = "Attach to testhost",
        request = "attach",
        processId = pid,
    })
end

return M
