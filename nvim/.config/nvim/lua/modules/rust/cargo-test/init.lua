-- Rust test runner: builds `cargo nextest run` commands for each task.test_type.
--
-- REQUIRED DEPENDENCIES (see lua/modules/rust/RUST_TESTS.md for details):
--   * cargo-nextest    `cargo install cargo-nextest --locked`
--   * rust-analyzer    LSP, for experimental/runnables (test discovery at cursor)
--   * codelldb         DAP adapter, for <leader>td test debug
--
-- We only use `cargo nextest run` — never `cargo test`. Nextest emits structured
-- JUnit XML which our pipeline parses for signs, diagnostics, and the tree view.
-- If nextest is missing, the runner aborts with a clear install message rather
-- than silently falling back to libtest stdout parsing.
--
-- Approach:
--   * For CURRENT_TEST and FILE_TESTS, query rust-analyzer's `experimental/runnables`
--     at the cursor / file. Rust-analyzer returns the precise cargo invocation
--     (package, target kind, test name). We translate `cargo test ...` into
--     `cargo nextest run ...` and inject our junit profile config so XML reports
--     land in `<workspace_root>/target/nextest/default/junit.xml`.
--   * For ALL_TESTS / ALL_MODULES_TESTS / SELECTED_MODULES_TESTS / ALL_DIR_TESTS,
--     build the command directly from cargo metadata.
--   * Debug runs use `cargo test --no-run --message-format=json` to discover the
--     compiled test binary and then launch codelldb via nvim-dap.
--   * The junit reporter is injected via `--tool-config-file` (composable with
--     user's own .config/nextest.toml). The config file lives at:
--       ~/.cache/nvim/test-report/nextest.toml

local nio_util = require("utils.nio-util")
local lsp_util = require("utils.lsp-util")

local M = {}

local NEXTEST_TOOL_CONFIG_DIR = vim.fn.stdpath("cache") .. "/test-report"
local NEXTEST_TOOL_CONFIG_PATH = NEXTEST_TOOL_CONFIG_DIR .. "/nextest.toml"
local NEXTEST_TOOL_ID = "copilot-cli"

local _nextest_available
--- Detect whether `cargo nextest` is installed and usable.
--- Cached for the session.
---@return boolean
local function nextest_available()
    if _nextest_available ~= nil then
        return _nextest_available
    end
    local code = os.execute("cargo nextest --version >/dev/null 2>&1")
    -- os.execute returns true (Lua 5.2+) or 0 on success.
    _nextest_available = (code == true or code == 0)
    if not _nextest_available then
        vim.schedule(function()
            vim.notify(
                "cargo-nextest is not installed. Rust test report parsing requires nextest.\n"
                    .. "Install it with: cargo install cargo-nextest --locked\n"
                    .. "(libtest stdout fallback is not yet implemented.)",
                vim.log.levels.ERROR
            )
        end)
    end
    return _nextest_available
end

local function ensure_nextest_config()
    if vim.fn.filereadable(NEXTEST_TOOL_CONFIG_PATH) == 1 then
        return NEXTEST_TOOL_CONFIG_PATH
    end
    vim.fn.mkdir(NEXTEST_TOOL_CONFIG_DIR, "p")
    local f = io.open(NEXTEST_TOOL_CONFIG_PATH, "w")
    if not f then
        return nil
    end
    f:write('[profile.default.junit]\npath = "junit.xml"\n')
    f:close()
    return NEXTEST_TOOL_CONFIG_PATH
end

---@return string|nil
local function cargo_workspace_root()
    local cwd = vim.fn.getcwd()
    local cmd = string.format(
        "cd %s 2>/dev/null && cargo metadata --no-deps --format-version 1 2>/dev/null",
        vim.fn.shellescape(cwd)
    )
    local handle = io.popen(cmd)
    if not handle then
        return nil
    end
    local out = handle:read("*a")
    handle:close()
    local ok, meta = pcall(vim.fn.json_decode, out)
    if not ok or type(meta) ~= "table" then
        return nil
    end
    return meta.workspace_root
end

---@return table[]
local function cargo_metadata_packages(workspace_root)
    local cmd = string.format(
        "cd %s 2>/dev/null && cargo metadata --no-deps --format-version 1 2>/dev/null",
        vim.fn.shellescape(workspace_root or vim.fn.getcwd())
    )
    local handle = io.popen(cmd)
    if not handle then
        return {}, nil
    end
    local out = handle:read("*a")
    handle:close()
    local ok, meta = pcall(vim.fn.json_decode, out)
    if not ok or type(meta) ~= "table" then
        return {}, nil
    end
    return meta.packages or {}, meta.workspace_members or {}
end

---@param ws_root string
---@return { name: string, path: string, manifest_path: string }[]
local function workspace_member_packages(ws_root)
    local packages, members = cargo_metadata_packages(ws_root)
    local member_ids = {}
    for _, m in ipairs(members or {}) do
        member_ids[m] = true
    end
    local result = {}
    for _, pkg in ipairs(packages) do
        if member_ids[pkg.id] then
            table.insert(result, {
                name = pkg.name,
                path = vim.fn.fnamemodify(pkg.manifest_path, ":h"),
                manifest_path = pkg.manifest_path,
            })
        end
    end
    return result
end

--- Locate the package containing the current file by walking up to Cargo.toml.
---@return { name: string, path: string }|nil
local function current_package()
    local file = vim.fn.expand("%:p")
    if file == "" then
        return nil
    end
    local dir = vim.fn.fnamemodify(file, ":h")
    local manifest = vim.fs.find("Cargo.toml", { upward = true, path = dir, type = "file" })[1]
    if not manifest then
        return nil
    end
    local pkg_dir = vim.fn.fnamemodify(manifest, ":h")
    local ws = cargo_workspace_root() or pkg_dir
    for _, pkg in ipairs(workspace_member_packages(ws)) do
        if pkg.manifest_path == manifest then
            return pkg
        end
    end
    return { name = vim.fn.fnamemodify(pkg_dir, ":t"), path = pkg_dir }
end

--- Get a rust-analyzer client (first one found).
---@return vim.lsp.Client|nil
local function rust_analyzer_client()
    return lsp_util.get_client_by_name("rust-analyzer", { bufnr = 0 }) or lsp_util.get_client_by_name("rust-analyzer")
end

--- Request rust-analyzer `experimental/runnables` for a given position.
--- pos may be nil to get all runnables in the file.
---@param pos { line: integer, character: integer }|nil
---@return table[]|nil
local function lsp_runnables(pos)
    local client = rust_analyzer_client()
    if not client then
        return nil
    end
    local uri = vim.uri_from_bufnr(0)
    local params = {
        textDocument = { uri = uri },
        position = pos,
    }
    local resp = client:request_sync("experimental/runnables", params, 5000, 0)
    if not resp or resp.err or not resp.result then
        return nil
    end
    return resp.result
end

--- Pick the most specific cargo-test runnable matching a predicate.
---@param runnables table[]
---@param pred fun(r: table): boolean
---@return table|nil
local function pick_runnable(runnables, pred)
    for _, r in ipairs(runnables or {}) do
        if r.kind == "cargo" and r.args and r.args.cargoArgs and r.args.cargoArgs[1] == "test" and pred(r) then
            return r
        end
    end
    return nil
end

--- Transform a rust-analyzer cargo-test runnable into a `cargo nextest run` cmd.
--- Strips libtest-only flags. Injects junit tool-config.
---@param runnable table
---@return string[]
local function runnable_to_nextest_cmd(runnable)
    local cargo_args = vim.deepcopy(runnable.args.cargoArgs or {})
    local exe_args = runnable.args.executableArgs or {}

    -- Replace "test" subcommand with "nextest run".
    -- cargo_args[1] is always "test" here.
    table.remove(cargo_args, 1)

    -- Strip libtest-specific flags rust-analyzer often adds.
    local cargo_args_clean = {}
    local skip_next = false
    for _, a in ipairs(cargo_args) do
        if skip_next then
            skip_next = false
        elseif a == "--nocapture" or a == "--show-output" or a == "--quiet" then
            -- libtest flag, drop
        elseif a == "--test-threads" or a == "-Z" then
            skip_next = true
        else
            table.insert(cargo_args_clean, a)
        end
    end

    -- Build positional filter list from executableArgs (test name + --exact).
    local filters = {}
    local exact = false
    for _, a in ipairs(exe_args) do
        if a == "--exact" then
            exact = true
        elseif a:sub(1, 1) ~= "-" then
            table.insert(filters, a)
        end
    end

    local tool_config = ensure_nextest_config()
    local cmd = { "cargo", "nextest", "run" }
    if tool_config then
        table.insert(cmd, "--tool-config-file")
        table.insert(cmd, NEXTEST_TOOL_ID .. ":" .. tool_config)
    end
    table.insert(cmd, "--no-fail-fast")

    for _, a in ipairs(cargo_args_clean) do
        table.insert(cmd, a)
    end

    -- nextest filter syntax: use filterset expressions for exact match.
    -- `--exact` is a libtest flag and isn't accepted by nextest.
    -- For non-exact filters, pass the name as a positional (substring match).
    for _, f in ipairs(filters) do
        if exact then
            table.insert(cmd, "-E")
            table.insert(cmd, "test(=" .. f .. ")")
        else
            table.insert(cmd, f)
        end
    end

    return cmd
end

--- Build a workspace-level nextest cmd.
---@param opts { package?: string, args?: string[] }|nil
---@return string[]
local function build_workspace_cmd(opts)
    opts = opts or {}
    local tool_config = ensure_nextest_config()
    local cmd = { "cargo", "nextest", "run" }
    if tool_config then
        table.insert(cmd, "--tool-config-file")
        table.insert(cmd, NEXTEST_TOOL_ID .. ":" .. tool_config)
    end
    table.insert(cmd, "--no-fail-fast")
    if opts.package then
        table.insert(cmd, "-p")
        table.insert(cmd, opts.package)
    else
        table.insert(cmd, "--workspace")
    end
    for _, a in ipairs(opts.args or {}) do
        table.insert(cmd, a)
    end
    return cmd
end

--- Build a chained shell command running tests for several packages, each
--- writing its own junit.xml under that package's target dir.
---@param packages { name: string, path: string }[]
---@return string|nil cmd_string, string[] report_dirs
local function build_multi_package_cmd(packages)
    if not packages or #packages == 0 then
        return nil, {}
    end
    local tool_config = ensure_nextest_config()
    local parts = { "r=0" }
    local report_dirs = {}
    for _, pkg in ipairs(packages) do
        local cmd = string.format(
            "cd %s && cargo nextest run --tool-config-file %s:%s --no-fail-fast -p %s",
            vim.fn.shellescape(pkg.path),
            NEXTEST_TOOL_ID,
            vim.fn.shellescape(tool_config or ""),
            vim.fn.shellescape(pkg.name)
        )
        table.insert(parts, cmd .. " || r=1")
        table.insert(report_dirs, pkg.path .. "/target/nextest/default")
    end
    table.insert(parts, "exit $r")
    return table.concat(parts, "; "), report_dirs
end

--------------------------------------------------------------------------------
-- Per-test-type resolver

local state = {}

---@param context task.lang.Context
---@return string[]|nil cmd
---@return string|string[]|nil report_dir
local function resolve_cmd(context)
    local t = context.test_type
    local ws = cargo_workspace_root()
    local default_report_dir = ws and (ws .. "/target/nextest/default") or nil

    if t == task.test_type.ALL_TESTS or t == task.test_type.ALL_MODULES_TESTS then
        return build_workspace_cmd(), default_report_dir
    end

    if t == task.test_type.SELECTED_MODULES_TESTS then
        -- Selection must be done upfront (in the keymap's nio.run context) and
        -- passed via context.selected_packages, because by the time our builder
        -- runs, overseer's built-in cargo template has already async-broken the
        -- nio context (calling nio_util.multi_select here errors with
        -- "Cannot call async function from non-async context").
        local picked = context.selected_packages
        if not picked or #picked == 0 then
            vim.notify("No packages selected", vim.log.levels.WARN)
            return nil, nil
        end
        local cmd_str, dirs = build_multi_package_cmd(picked)
        return cmd_str and { "sh", "-c", cmd_str } or nil, dirs
    end

    if t == task.test_type.ALL_DIR_TESTS then
        local pkg = current_package()
        if not pkg then
            vim.notify("Could not resolve current Cargo package", vim.log.levels.WARN)
            return nil, nil
        end
        local cmd = build_workspace_cmd({ package = pkg.name })
        return cmd, pkg.path .. "/target/nextest/default"
    end

    if t == task.test_type.FILE_TESTS then
        local runnables = lsp_runnables(nil)
        if not runnables then
            vim.notify("rust-analyzer runnables unavailable; falling back to package run", vim.log.levels.WARN)
            local pkg = current_package()
            if pkg then
                return build_workspace_cmd({ package = pkg.name }), pkg.path .. "/target/nextest/default"
            end
            return nil, nil
        end
        -- Prefer a "test-mod" or "test" runnable that has NO --exact (i.e. file/mod scope).
        local file_runnable = pick_runnable(runnables, function(r)
            local exe = r.args.executableArgs or {}
            for _, a in ipairs(exe) do
                if a == "--exact" then
                    return false
                end
            end
            return true
        end)
        if not file_runnable then
            -- Fallback: take any test runnable
            file_runnable = pick_runnable(runnables, function()
                return true
            end)
        end
        if not file_runnable then
            vim.notify("No test runnable found in file", vim.log.levels.WARN)
            return nil, nil
        end
        state.last = {
            kind = "runnable",
            runnable = file_runnable,
            test_type = t,
            bufnr = vim.api.nvim_get_current_buf(),
        }
        return runnable_to_nextest_cmd(file_runnable), default_report_dir
    end

    if t == task.test_type.CURRENT_TEST then
        local pos_params = vim.lsp.util.make_position_params(0, "utf-8")
        local runnables = lsp_runnables(pos_params.position)
        if not runnables then
            vim.notify("rust-analyzer runnables unavailable", vim.log.levels.WARN)
            return nil, nil
        end
        -- Pick the runnable with --exact (most specific = single test).
        local cur = pick_runnable(runnables, function(r)
            local exe = r.args.executableArgs or {}
            for _, a in ipairs(exe) do
                if a == "--exact" then
                    return true
                end
            end
            return false
        end)
        if not cur then
            vim.notify("No test at cursor", vim.log.levels.WARN)
            return nil, nil
        end
        state.last = {
            kind = "runnable",
            runnable = cur,
            test_type = t,
            bufnr = vim.api.nvim_get_current_buf(),
        }
        return runnable_to_nextest_cmd(cur), default_report_dir
    end

    if t == task.test_type.CURRENT_PARAMETRIZED_NUM_TEST then
        vim.notify("Parametrized single-test selection is not implemented for Rust", vim.log.levels.WARN)
        return nil, nil
    end

    if t == task.test_type.TOGGLE_LAST_DEBUG then
        if not state.last or state.last.kind ~= "runnable" then
            vim.notify("No previous rust test to toggle debug for", vim.log.levels.WARN)
            return nil, nil
        end
        return runnable_to_nextest_cmd(state.last.runnable), default_report_dir
    end

    vim.notify("Unsupported rust test type: " .. tostring(t), vim.log.levels.WARN)
    return nil, nil
end

--------------------------------------------------------------------------------
-- Debug via codelldb + nvim-dap.
-- For Rust we run `cargo test --no-run --message-format=json` to discover the
-- compiled test binary, then launch codelldb against it with the test name.

---@param runnable table
---@return string|nil binary_path, string[] dap_args
local function compile_and_locate_test_binary(runnable)
    local cargo_args = vim.deepcopy(runnable.args.cargoArgs or {})
    -- cargo_args starts with "test". Inject --no-run and json output.
    -- We need to filter out positional test name and -- separator so cargo doesn't
    -- try to run anything.
    local filtered = {}
    local seen_dashdash = false
    for _, a in ipairs(cargo_args) do
        if a == "--" then
            seen_dashdash = true
        elseif not seen_dashdash then
            table.insert(filtered, a)
        end
    end
    -- ensure --no-run and json
    table.insert(filtered, "--no-run")
    table.insert(filtered, "--message-format=json")

    local cwd = runnable.args.workspaceRoot or vim.fn.getcwd()
    local cmd = { "cargo" }
    vim.list_extend(cmd, filtered)

    local sysobj = vim.system(cmd, { cwd = cwd, text = true }):wait()
    local out = (sysobj and sysobj.stdout) or ""

    local binary
    for line in out:gmatch("[^\n]+") do
        local ok, msg = pcall(vim.fn.json_decode, line)
        if ok and type(msg) == "table" and msg.reason == "compiler-artifact" and msg.executable and msg.profile then
            if msg.profile.test then
                binary = msg.executable
            end
        end
    end

    -- Compute the DAP-side args from executableArgs (drop libtest-only).
    local dap_args = {}
    for _, a in ipairs(runnable.args.executableArgs or {}) do
        if a ~= "--nocapture" and a ~= "--show-output" then
            table.insert(dap_args, a)
        end
    end
    return binary, dap_args
end

---@param context task.lang.Context
function M.dap_launch_test(context)
    local t = context.test_type
    local runnable
    if t == task.test_type.TOGGLE_LAST_DEBUG and state.last then
        runnable = state.last.runnable
    elseif t == task.test_type.CURRENT_TEST then
        local pos = vim.lsp.util.make_position_params(0, "utf-8").position
        local rs = lsp_runnables(pos) or {}
        runnable = pick_runnable(rs, function(r)
            for _, a in ipairs(r.args.executableArgs or {}) do
                if a == "--exact" then
                    return true
                end
            end
            return false
        end)
    elseif t == task.test_type.FILE_TESTS then
        local rs = lsp_runnables(nil) or {}
        runnable = pick_runnable(rs, function()
            return true
        end)
    else
        vim.notify("Debug not supported for rust test type: " .. tostring(t), vim.log.levels.WARN)
        return
    end
    if not runnable then
        vim.notify("No rust test runnable found to debug", vim.log.levels.WARN)
        return
    end

    -- Remember the selection (mode-agnostic) so a debug run can be toggled back
    -- to a regular run via TOGGLE_LAST_DEBUG (<leader>tD / <leader>tl), mirroring
    -- the non-debug resolve_cmd path.
    if t ~= task.test_type.TOGGLE_LAST_DEBUG then
        state.last = {
            kind = "runnable",
            runnable = runnable,
            test_type = t,
            bufnr = vim.api.nvim_get_current_buf(),
        }
    end

    nio_util.run(function()
        vim.notify("Compiling test binary for debugging...", vim.log.levels.INFO)
        local binary, dap_args = compile_and_locate_test_binary(runnable)
        if not binary then
            vim.notify("Could not locate compiled test binary", vim.log.levels.ERROR)
            return
        end
        vim.schedule(function()
            require("dap").run({
                name = "Rust: Debug test",
                type = "codelldb",
                request = "launch",
                program = binary,
                cwd = runnable.args.workspaceRoot or vim.fn.getcwd(),
                stopOnEntry = false,
                args = dap_args,
                terminal = "integrated",
                sourceLanguages = { "rust" },
                expressions = "native",
            })
        end)
    end)
end

--------------------------------------------------------------------------------

--- Hook invoked from overseer-util.run_test BEFORE the overseer task is built.
--- We're still in the keymap's nio.run() context here, so async prompts work.
--- We populate context.selected_packages so the builder doesn't need to prompt.
---@param context task.lang.Context
---@return boolean ok, string|nil err  (return false to abort the run)
function M.prepare_test_context(context)
    if context.is_debug then
        return true
    end
    if context.test_type == task.test_type.SELECTED_MODULES_TESTS then
        local ws = cargo_workspace_root() or vim.fn.getcwd()
        local packages = workspace_member_packages(ws)
        if #packages == 0 then
            return false, "No workspace members found"
        end
        local picked = nio_util.multi_select(packages, "Select packages to test")
        if not picked or #picked == 0 then
            return false, "No packages selected"
        end
        context.selected_packages = picked
    end
    return true
end

---@param context task.lang.Context
---@return task.lang.test.TestCmd
function M.build_run_test_cmd(context)
    if not nextest_available() then
        return { cmd = { "echo", "cargo-nextest not installed; aborting." } }
    end

    -- Note: is_debug is handled by overseer-util.run_test calling M.dap_launch_test
    -- directly (we expose dap_launch_test below). This builder is only invoked for
    -- non-debug runs.
    local cmd, report_dir = resolve_cmd(context)
    if not cmd then
        return { cmd = { "echo", "Could not build rust test command" } }
    end
    return { cmd = cmd, report_dir = report_dir }
end

---@return string|nil
function M.get_test_report_dir()
    local ws = cargo_workspace_root()
    if not ws then
        return nil
    end
    return ws .. "/target/nextest/default"
end

return M
