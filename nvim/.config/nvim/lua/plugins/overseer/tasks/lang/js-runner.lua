local M = {}

---@return table
function M.build_run_cmd()
    local file = vim.fn.expand("%:p")
    return { "deno", "run", file }
end

-- Debug plain js/ts scripts via Deno + the pwa-node (js-debug) adapter.
-- Deno runs both .js and .ts natively, so a single config covers both.
-- `--inspect-wait` makes Deno pause until the debugger attaches; js-debug
-- attaches to the inspector via `attachSimplePort`.
function M.dap_launch()
    require("dap").run({
        type = "pwa-node",
        request = "launch",
        name = "Deno: Launch file",
        runtimeExecutable = "deno",
        runtimeArgs = { "run", "--inspect-wait", "--allow-all" },
        program = "${file}",
        cwd = "${workspaceFolder}",
        attachSimplePort = 9229,
        sourceMaps = true,
        skipFiles = {
            "<node_internals>/**",
            "node_modules/**",
        },
        resolveSourceMapLocations = {
            "${workspaceFolder}/**",
            "!**/node_modules/**",
        },
    })
end

function M.dap_launch_rerun()
    M.dap_launch()
end

--------------------------------------------------------------------------------
-- jest test integration (delegates to modules.js.jest-test).
-- Works for both JavaScript and TypeScript projects (ts-jest transpiles `.ts`).

---@param context task.lang.Context
---@return boolean ok, string|nil err
function M.prepare_test_context(context)
    return require("modules.js.jest-test").prepare_test_context(context)
end

---@param context task.lang.Context
---@return task.lang.test.TestCmd
function M.build_run_test_cmd(context)
    return require("modules.js.jest-test").build_run_test_cmd(context)
end

---@param context task.lang.Context
function M.dap_launch_test(context)
    return require("modules.js.jest-test").dap_launch_test(context)
end

---@return string|nil
function M.get_test_report_dir()
    return require("modules.js.jest-test").get_test_report_dir()
end

return M
