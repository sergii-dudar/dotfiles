-- Go test runner: builds `go test -json` commands for each task.test_type.
--
-- REQUIRED DEPENDENCIES (see lua/modules/go/GO_TESTS.md for details):
--   * go              the Go toolchain (built-in `go test -json` for streamed events)
--   * delve (dlv)     DAP adapter, for <leader>td test debug
--                     `go install github.com/go-delve/delve/cmd/dlv@latest`
--
-- We always use `go test -json` and pipe the NDJSON stream into a file under
--   $XDG_CACHE_HOME/nvim/test-report/go/<safe-module-path>/go-test.json
-- which our pipeline parses for signs, diagnostics, and the tree view.
--
-- Approach:
--   * For CURRENT_TEST / FILE_TESTS, locate the current file's package directory
--     (relative to module root) and use `-run '^(T1|T2|...)$'` with names found
--     via treesitter inside the language adapter.
--   * For ALL_TESTS / ALL_MODULES_TESTS / ALL_DIR_TESTS, run with `./...` from
--     module root (or `./<rel_dir>/...`).
--   * SELECTED_MODULES_TESTS prompts via prepare_test_context for go.work module
--     selection (or individual packages if not a workspace).
--   * Debug runs invoke delve via nvim-dap with mode=test.

local nio_util = require("utils.nio-util")

local M = {}

---@return string|nil
local function go_module_root(cwd)
    cwd = cwd or vim.fn.getcwd()
    local cmd = string.format("cd %s 2>/dev/null && go env GOMOD 2>/dev/null", vim.fn.shellescape(cwd))
    local h = io.popen(cmd)
    if not h then
        return nil
    end
    local out = (h:read("*a") or ""):gsub("\n", "")
    h:close()
    if out == "" or out == "/dev/null" then
        return nil
    end
    return vim.fn.fnamemodify(out, ":h")
end

---@param module_root string
---@return string
local function report_dir_for(module_root)
    local safe = module_root:gsub("[/\\]", "_"):gsub("^_+", "")
    return vim.fn.stdpath("cache") .. "/test-report/go/" .. safe
end

---@return string|nil
function M.get_test_report_dir()
    local root = go_module_root()
    if not root then
        return nil
    end
    return report_dir_for(root)
end

local _go_available
local function go_available()
    if _go_available ~= nil then
        return _go_available
    end
    _go_available = (os.execute("go version >/dev/null 2>&1") == true)
        or (os.execute("go version >/dev/null 2>&1") == 0)
    if not _go_available then
        vim.schedule(function()
            vim.notify("Go test runner: `go` binary not found in PATH.", vim.log.levels.ERROR)
        end)
    end
    return _go_available
end

local _dlv_available
local function dlv_available()
    if _dlv_available ~= nil then
        return _dlv_available
    end
    _dlv_available = (os.execute("dlv version >/dev/null 2>&1") == true)
        or (os.execute("dlv version >/dev/null 2>&1") == 0)
    return _dlv_available
end

--- Get current file's package directory absolute path (walk up until *.go in a
--- directory that is inside the module root).
---@return string|nil pkg_dir, string|nil module_root
local function current_package_dir()
    local file = vim.fn.expand("%:p")
    if file == "" then
        return nil, nil
    end
    local module_root = go_module_root(vim.fn.fnamemodify(file, ":h"))
    if not module_root then
        return nil, nil
    end
    return vim.fn.fnamemodify(file, ":h"), module_root
end

--- Convert an absolute pkg dir to a `./<rel>/` form relative to module_root.
---@param pkg_dir string
---@param module_root string
---@return string
local function rel_pkg_pattern(pkg_dir, module_root)
    local abs_pkg = vim.fn.fnamemodify(pkg_dir, ":p"):gsub("/$", "")
    local abs_root = vim.fn.fnamemodify(module_root, ":p"):gsub("/$", "")
    if abs_pkg == abs_root then
        return "."
    end
    if vim.startswith(abs_pkg, abs_root .. "/") then
        return "./" .. abs_pkg:sub(#abs_root + 2)
    end
    return abs_pkg
end

--- Treesitter scan for test function names in current buffer (only Test*/Benchmark*/Example*).
---@return string[]
local function test_fns_in_current_buffer()
    local bufnr = vim.api.nvim_get_current_buf()
    local ok, parser = pcall(vim.treesitter.get_parser, bufnr, "go")
    if not ok or not parser then
        return {}
    end
    local tree = parser:parse()[1]
    if not tree then
        return {}
    end
    local query = vim.treesitter.query.parse(
        "go",
        [[
        (function_declaration
          name: (identifier) @fn.name
          parameters: (parameter_list) @fn.params
        )
    ]]
    )
    local names = {}
    local capture_ids = {}
    for i, name in ipairs(query.captures) do
        capture_ids[name] = i
    end
    for _, match in query:iter_matches(tree:root(), bufnr, 0, -1, { all = true }) do
        local name_node = match[capture_ids["fn.name"]] and match[capture_ids["fn.name"]][1]
        local params_node = match[capture_ids["fn.params"]] and match[capture_ids["fn.params"]][1]
        if name_node and params_node then
            local fn_name = vim.treesitter.get_node_text(name_node, bufnr)
            local params_text = vim.treesitter.get_node_text(params_node, bufnr)
            if
                (
                    fn_name:match("^Test[%u_]")
                    or fn_name == "Test"
                    or fn_name:match("^Benchmark")
                    or fn_name:match("^Example")
                ) and params_text:match("%*testing%.[TBF]%f[^%a]")
            then
                table.insert(names, fn_name)
            end
        end
    end
    return names
end

--- Find the Test fn at cursor (closest function_declaration enclosing the cursor).
---@return string|nil
local function test_fn_at_cursor()
    local bufnr = vim.api.nvim_get_current_buf()
    local cursor = vim.api.nvim_win_get_cursor(0)
    local row = cursor[1] - 1
    local ok, parser = pcall(vim.treesitter.get_parser, bufnr, "go")
    if not ok or not parser then
        return nil
    end
    local tree = parser:parse()[1]
    if not tree then
        return nil
    end
    local node = tree:root():descendant_for_range(row, 0, row, 0)
    while node do
        if node:type() == "function_declaration" then
            for child in node:iter_children() do
                if child:type() == "identifier" then
                    local name = vim.treesitter.get_node_text(child, bufnr)
                    if name:match("^Test") or name:match("^Benchmark") or name:match("^Example") then
                        return name
                    end
                    return nil
                end
            end
            return nil
        end
        node = node:parent()
    end
    return nil
end

--- Discover go.work modules (or fall back to single-module list).
---@param module_root string
---@return { name: string, path: string }[]
local function workspace_modules(module_root)
    -- Try `go work edit -json` first.
    local cmd = string.format("cd %s 2>/dev/null && go env GOWORK 2>/dev/null", vim.fn.shellescape(module_root))
    local h = io.popen(cmd)
    if h then
        local gowork = (h:read("*a") or ""):gsub("\n", "")
        h:close()
        if gowork ~= "" and gowork ~= "off" then
            local work_dir = vim.fn.fnamemodify(gowork, ":h")
            local cmd2 =
                string.format("cd %s 2>/dev/null && go work edit -json 2>/dev/null", vim.fn.shellescape(work_dir))
            local h2 = io.popen(cmd2)
            if h2 then
                local out = h2:read("*a")
                h2:close()
                local ok, data = pcall(vim.fn.json_decode, out or "")
                if ok and type(data) == "table" and data.Use then
                    local result = {}
                    for _, u in ipairs(data.Use) do
                        local path = u.DiskPath or ""
                        if not path:match("^/") then
                            path = work_dir .. "/" .. path
                        end
                        path = vim.fn.fnamemodify(path, ":p"):gsub("/$", "")
                        table.insert(result, { name = vim.fn.fnamemodify(path, ":t"), path = path })
                    end
                    if #result > 0 then
                        return result
                    end
                end
            end
        end
    end
    return { { name = vim.fn.fnamemodify(module_root, ":t"), path = module_root } }
end

--------------------------------------------------------------------------------
-- Command builders

--- Build the shell command running `go test -json` and capturing into a file.
--- The cmd is wrapped in `sh -c` so we get tee + report-dir clearing.
---@param opts { module_root: string, pkg_pattern: string, run_regex?: string, report_dir: string, extra_args?: string[] }
---@return string[] cmd, string report_dir
local function build_shell_cmd(opts)
    local report_dir = opts.report_dir
    local report_file = report_dir .. "/go-test.json"
    local args = { "go", "test", "-json" }
    if opts.run_regex then
        table.insert(args, "-run")
        table.insert(args, opts.run_regex)
    end
    if opts.extra_args then
        for _, a in ipairs(opts.extra_args) do
            table.insert(args, a)
        end
    end
    table.insert(args, opts.pkg_pattern)

    -- Use single-quoting for shell-safety on the regex (already constrained chars).
    local quoted = {}
    for _, a in ipairs(args) do
        table.insert(quoted, vim.fn.shellescape(a))
    end

    local cmd_str = string.format(
        "cd %s && mkdir -p %s && rm -f %s && %s 2>&1 | tee %s; exit ${PIPESTATUS[0]}",
        vim.fn.shellescape(opts.module_root),
        vim.fn.shellescape(report_dir),
        vim.fn.shellescape(report_file),
        table.concat(quoted, " "),
        vim.fn.shellescape(report_file)
    )
    -- ${PIPESTATUS} requires bash; use bash explicitly.
    return { "bash", "-c", cmd_str }, report_dir
end

--- Build cmd that runs tests across several module roots, concatenating reports.
---@param modules { name: string, path: string }[]
---@return string[]|nil cmd, string[] report_dirs
local function build_multi_module_cmd(modules)
    if not modules or #modules == 0 then
        return nil, {}
    end
    local parts = { "r=0" }
    local dirs = {}
    for _, mod in ipairs(modules) do
        local rd = report_dir_for(mod.path)
        local rf = rd .. "/go-test.json"
        table.insert(dirs, rd)
        local snippet = string.format(
            "(cd %s && mkdir -p %s && rm -f %s && go test -json ./... 2>&1 | tee %s) || r=1",
            vim.fn.shellescape(mod.path),
            vim.fn.shellescape(rd),
            vim.fn.shellescape(rf),
            vim.fn.shellescape(rf)
        )
        table.insert(parts, snippet)
    end
    table.insert(parts, "exit $r")
    return { "bash", "-c", table.concat(parts, "; ") }, dirs
end

--- Escape regex meta characters for use inside Go's -run regex.
---@param s string
local function regex_escape(s)
    return (s:gsub("([%.%^%$%(%)%%%+%-%*%?%[%]%{%}|\\])", "%%%1"))
end

--------------------------------------------------------------------------------
-- Per-test-type resolver

local state = {}

---@param context task.lang.Context
---@return string[]|nil cmd
---@return string|string[]|nil report_dir
local function resolve_cmd(context)
    local t = context.test_type
    local module_root = go_module_root()
    if not module_root then
        vim.notify("Go test runner: not inside a Go module", vim.log.levels.WARN)
        return nil, nil
    end
    local report_dir = report_dir_for(module_root)

    if t == task.test_type.ALL_TESTS or t == task.test_type.ALL_MODULES_TESTS then
        return build_shell_cmd({ module_root = module_root, pkg_pattern = "./...", report_dir = report_dir })
    end

    if t == task.test_type.SELECTED_MODULES_TESTS then
        local picked = context.selected_packages
        if not picked or #picked == 0 then
            vim.notify("No modules selected", vim.log.levels.WARN)
            return nil, nil
        end
        local cmd, dirs = build_multi_module_cmd(picked)
        return cmd, dirs
    end

    if t == task.test_type.ALL_DIR_TESTS then
        local pkg_dir = current_package_dir()
        if not pkg_dir then
            vim.notify("Could not resolve current Go package", vim.log.levels.WARN)
            return nil, nil
        end
        local pattern = rel_pkg_pattern(pkg_dir, module_root)
        return build_shell_cmd({
            module_root = module_root,
            pkg_pattern = pattern == "." and "./..." or (pattern .. "/..."),
            report_dir = report_dir,
        })
    end

    if t == task.test_type.FILE_TESTS then
        local pkg_dir = current_package_dir()
        if not pkg_dir then
            return nil, nil
        end
        local pattern = rel_pkg_pattern(pkg_dir, module_root)
        local fns = test_fns_in_current_buffer()
        if #fns == 0 then
            vim.notify("No tests in current file", vim.log.levels.WARN)
            return nil, nil
        end
        local escaped = {}
        for _, f in ipairs(fns) do
            table.insert(escaped, regex_escape(f))
        end
        local run_regex = "^(" .. table.concat(escaped, "|") .. ")$"
        state.last = { kind = "file", pkg_pattern = pattern, run_regex = run_regex, test_type = t }
        return build_shell_cmd({
            module_root = module_root,
            pkg_pattern = pattern,
            run_regex = run_regex,
            report_dir = report_dir,
        })
    end

    if t == task.test_type.CURRENT_TEST then
        local pkg_dir = current_package_dir()
        local fn = test_fn_at_cursor()
        if not pkg_dir or not fn then
            vim.notify("No test at cursor", vim.log.levels.WARN)
            return nil, nil
        end
        local pattern = rel_pkg_pattern(pkg_dir, module_root)
        local run_regex = "^" .. regex_escape(fn) .. "$"
        state.last = { kind = "single", pkg_pattern = pattern, run_regex = run_regex, fn = fn, test_type = t }
        return build_shell_cmd({
            module_root = module_root,
            pkg_pattern = pattern,
            run_regex = run_regex,
            report_dir = report_dir,
        })
    end

    if t == task.test_type.CURRENT_PARAMETRIZED_NUM_TEST then
        vim.notify("Parametrized single-test selection is not implemented for Go", vim.log.levels.WARN)
        return nil, nil
    end

    if t == task.test_type.TOGGLE_LAST_DEBUG then
        if not state.last then
            vim.notify("No previous go test to toggle debug for", vim.log.levels.WARN)
            return nil, nil
        end
        return build_shell_cmd({
            module_root = module_root,
            pkg_pattern = state.last.pkg_pattern,
            run_regex = state.last.run_regex,
            report_dir = report_dir,
        })
    end

    vim.notify("Unsupported go test type: " .. tostring(t), vim.log.levels.WARN)
    return nil, nil
end

--------------------------------------------------------------------------------
-- Debug via delve + nvim-dap.

---@param context task.lang.Context
function M.dap_launch_test(context)
    if not dlv_available() then
        vim.notify(
            "delve not installed. Install with:\n  go install github.com/go-delve/delve/cmd/dlv@latest",
            vim.log.levels.ERROR
        )
        return
    end

    local t = context.test_type
    local pkg_dir, module_root = current_package_dir()
    if not pkg_dir or not module_root then
        vim.notify("Not inside a Go module", vim.log.levels.WARN)
        return
    end

    local run_regex
    local fn_label
    if t == task.test_type.CURRENT_TEST then
        local fn = test_fn_at_cursor()
        if not fn then
            vim.notify("No test at cursor", vim.log.levels.WARN)
            return
        end
        run_regex = "^" .. regex_escape(fn) .. "$"
        fn_label = fn
    elseif t == task.test_type.FILE_TESTS then
        local fns = test_fns_in_current_buffer()
        if #fns == 0 then
            vim.notify("No tests in current file", vim.log.levels.WARN)
            return
        end
        local escaped = {}
        for _, f in ipairs(fns) do
            table.insert(escaped, regex_escape(f))
        end
        run_regex = "^(" .. table.concat(escaped, "|") .. ")$"
        fn_label = "file tests"
    elseif t == task.test_type.TOGGLE_LAST_DEBUG then
        if not state.last or not state.last.run_regex then
            vim.notify("No previous go test to toggle debug for", vim.log.levels.WARN)
            return
        end
        run_regex = state.last.run_regex
        fn_label = state.last.fn or "last"
    else
        vim.notify("Debug not supported for go test type: " .. tostring(t), vim.log.levels.WARN)
        return
    end

    state.last = state.last or {}
    state.last.run_regex = run_regex
    state.last.pkg_pattern = rel_pkg_pattern(pkg_dir, module_root)
    state.last.fn = fn_label

    require("dap").run({
        name = "Go: Debug test (" .. fn_label .. ")",
        type = "go",
        request = "launch",
        mode = "test",
        program = pkg_dir,
        args = { "-test.run", run_regex },
        cwd = pkg_dir,
    })
end

--------------------------------------------------------------------------------

---@param context task.lang.Context
---@return boolean ok, string|nil err
function M.prepare_test_context(context)
    if context.is_debug then
        return true
    end
    if context.test_type == task.test_type.SELECTED_MODULES_TESTS then
        local module_root = go_module_root() or vim.fn.getcwd()
        local mods = workspace_modules(module_root)
        if #mods == 0 then
            return false, "No Go modules found"
        end
        if #mods == 1 then
            -- Single module: no choice to make; auto-select.
            context.selected_packages = mods
            return true
        end
        local picked = nio_util.multi_select(mods, "Select Go modules to test")
        if not picked or #picked == 0 then
            return false, "No modules selected"
        end
        context.selected_packages = picked
    end
    return true
end

---@param context task.lang.Context
---@return task.lang.test.TestCmd
function M.build_run_test_cmd(context)
    if not go_available() then
        return { cmd = { "echo", "go binary not found; aborting." } }
    end
    local cmd, report_dir = resolve_cmd(context)
    if not cmd then
        return { cmd = { "echo", "Could not build go test command" } }
    end
    return { cmd = cmd, report_dir = report_dir }
end

return M
