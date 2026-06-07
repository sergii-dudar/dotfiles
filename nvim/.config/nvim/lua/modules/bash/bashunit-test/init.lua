-- Bash / bashunit test runner: builds `bashunit` commands for each task.test_type
-- and launches DAP via the mason bash-debug-adapter for <leader>td test debug.
--
-- REQUIRED:
--   * bashunit                    (auto-detected: project lib/bashunit -> $PATH -> /home/serhii/tools/tools/bashunit/bashunit)
--   * bash-debug-adapter          (Mason) — only for <leader>td test debug
--
-- bashunit emits JUnit XML via `--log-junit <file>` which our pipeline
-- (modules.bash.test-report) parses for signs / diagnostics / tree view.
--
-- Report file: $XDG_CACHE_HOME/nvim/test-report/bash/<safe-project-path>/junit.xml
--
-- Test types:
--   ALL_TESTS / ALL_MODULES_TESTS / SELECTED_MODULES_TESTS -> bashunit on project tests dir
--   ALL_DIR_TESTS                                          -> bashunit on current file's dir
--   FILE_TESTS                                             -> bashunit on the current file
--   CURRENT_TEST / CURRENT_PARAMETRIZED_NUM_TEST           -> bashunit <file> --filter <fn>
--   TOGGLE_LAST_DEBUG                                      -> rerun previous command (debug or not)

local constants = require("utils.constants")

local M = {}

local BASH_DEBUG_ADAPTER_PATH = vim.fn.stdpath("data") .. "/mason/packages/bash-debug-adapter/extension/bashdb_dir"

--------------------------------------------------------------------------------
-- Project root + bashunit binary resolution

local _root_cache = {}
local _bin_cache = {}

---@param start string
---@return string|nil
local function find_project_root(start)
    local dir = vim.fn.fnamemodify(start, ":p")
    if vim.fn.isdirectory(dir) == 0 then
        dir = vim.fn.fnamemodify(dir, ":h")
    end
    local file_markers = { ".bashunit", "bashunit.conf", "lib/bashunit", ".git" }
    local dir_markers = { "tests" }
    local prev = nil
    while dir ~= prev and dir ~= "/" and dir ~= "" do
        for _, m in ipairs(file_markers) do
            if vim.fn.filereadable(dir .. "/" .. m) == 1 or vim.fn.isdirectory(dir .. "/" .. m) == 1 then
                return dir
            end
        end
        for _, m in ipairs(dir_markers) do
            if vim.fn.isdirectory(dir .. "/" .. m) == 1 then
                return dir
            end
        end
        prev = dir
        dir = vim.fn.fnamemodify(dir, ":h")
    end
    return nil
end

---@return string
function M.project_root()
    local file = vim.fn.expand("%:p")
    local start = (file ~= "" and vim.fn.fnamemodify(file, ":h")) or vim.fn.getcwd()
    if _root_cache[start] ~= nil then
        return _root_cache[start] or vim.fn.getcwd()
    end
    local root = find_project_root(start) or vim.fn.getcwd()
    _root_cache[start] = root
    return root
end

--- Resolve the bashunit binary. Precedence:
---   1. `constants.bash.bashunit_bin` (manual system setting; recommended)
---   2. Project-local: <root>/lib/bashunit, <root>/bashunit
---   3. `bashunit` on $PATH (resolved to absolute via exepath)
---@return string|nil
function M.find_bashunit_bin()
    local configured = constants.bash and constants.bash.bashunit_bin
    if configured and configured ~= "" then
        if vim.fn.executable(configured) == 1 then
            return configured
        end
        vim.notify(
            "bashunit: configured path is not executable: " .. configured .. " — falling back to auto-detect",
            vim.log.levels.WARN
        )
    end

    local root = M.project_root()
    if _bin_cache[root] ~= nil then
        return _bin_cache[root] or nil
    end
    local candidates = { root .. "/lib/bashunit", root .. "/bashunit" }
    for _, c in ipairs(candidates) do
        if vim.fn.executable(c) == 1 then
            _bin_cache[root] = c
            return c
        end
    end
    if vim.fn.executable("bashunit") == 1 then
        local abs = vim.fn.exepath("bashunit")
        local resolved = (abs ~= "" and abs) or "bashunit"
        _bin_cache[root] = resolved
        return resolved
    end
    _bin_cache[root] = false
    return nil
end

---@param root string
---@return string
local function report_dir_for(root)
    local safe = root:gsub("[/\\]", "_"):gsub("^_+", "")
    return vim.fn.stdpath("cache") .. "/test-report/bash/" .. safe
end

---@return string
function M.get_test_report_dir()
    return report_dir_for(M.project_root())
end

---@return string
local function report_file()
    local dir = M.get_test_report_dir()
    vim.fn.mkdir(dir, "p")
    return dir .. "/junit.xml"
end

--- Wipe the report file so stale results don't leak into the next run.
local function reset_report_file()
    local f = report_file()
    -- truncate
    local h = io.open(f, "w")
    if h then
        h:close()
    end
    -- also remove sibling files (none expected, kept for symmetry)
end

--------------------------------------------------------------------------------
-- Treesitter test discovery (function names at cursor)

local _query
local function bash_test_query()
    if _query then
        return _query
    end
    _query = vim.treesitter.query.parse(
        "bash",
        [[
        (function_definition
          name: (word) @fn.name
          (#match? @fn.name "^test_?")
        ) @fn.def
    ]]
    )
    return _query
end

--- Return the test function name whose body contains the cursor, or nil.
---@param bufnr integer
---@param row integer 0-indexed
---@return string|nil fn_name, integer|nil start_row
function M.test_fn_at_row(bufnr, row)
    local ok, parser = pcall(vim.treesitter.get_parser, bufnr, "bash")
    if not ok or not parser then
        return nil
    end
    local tree = parser:parse()[1]
    if not tree then
        return nil
    end
    local query = bash_test_query()
    local match_name, match_start
    local best_span = math.huge
    for id, node in query:iter_captures(tree:root(), bufnr) do
        local capture = query.captures[id]
        if capture == "fn.def" then
            local sr, _, er, _ = node:range()
            if row >= sr and row <= er then
                local span = er - sr
                if span < best_span then
                    best_span = span
                    -- find the name child
                    for child in node:iter_children() do
                        if child:type() == "word" then
                            match_name = vim.treesitter.get_node_text(child, bufnr)
                            match_start = sr
                            break
                        end
                    end
                end
            end
        end
    end
    return match_name, match_start
end

--------------------------------------------------------------------------------
-- Command builders

local function abort_no_bashunit()
    return {
        cmd = {
            "echo",
            "bashunit not found. Install bashunit or place it at <project>/lib/bashunit, on $PATH, or at "
                .. FALLBACK_BASHUNIT,
        },
    }
end

---@param paths string|string[]
---@param extra string[]|nil
---@return string[]
local function build_cmd(paths, extra)
    local bin = M.find_bashunit_bin()
    local cmd = { bin, "--log-junit", report_file() }
    if extra then
        for _, a in ipairs(extra) do
            table.insert(cmd, a)
        end
    end
    if type(paths) == "string" then
        table.insert(cmd, paths)
    else
        for _, p in ipairs(paths) do
            table.insert(cmd, p)
        end
    end
    return cmd
end

---@return string[]  candidate test dirs under the project root, falling back to root
local function project_test_dirs()
    local root = M.project_root()
    local candidates = { "tests", "test", "spec" }
    local found = {}
    for _, c in ipairs(candidates) do
        local d = root .. "/" .. c
        if vim.fn.isdirectory(d) == 1 then
            table.insert(found, d)
        end
    end
    if #found == 0 then
        return { root }
    end
    return found
end

--- Memorised last selection (mode-agnostic) so TOGGLE_LAST_DEBUG can replay it
--- in either regular or debug mode.
local state = { last = nil }

--- Resolve the bashunit target selection for the given context. Shared by the
--- regular and debug builders so a run can be toggled between modes
--- (TOGGLE_LAST_DEBUG) reusing the exact same selection.
---
--- Returns `{ paths, extra }` where `paths` is a single file (string) or a list
--- of dirs, and `extra` is an optional `{ "--filter", fn }`. `for_debug` only
--- changes the no-test-under-cursor behaviour (regular falls back to the whole
--- file; debug aborts).
---@param context task.lang.Context
---@param opts { for_debug: boolean }
---@return { paths: string|string[], extra?: string[] }|nil
local function resolve_test_selection(context, opts)
    local t = context.test_type
    local file = vim.fn.expand("%:p")

    if t == task.test_type.TOGGLE_LAST_DEBUG then
        if state.last and state.last.paths then
            return { paths = state.last.paths, extra = state.last.extra }
        end
        return nil
    elseif
        t == task.test_type.ALL_TESTS
        or t == task.test_type.ALL_MODULES_TESTS
        or t == task.test_type.SELECTED_MODULES_TESTS
    then
        return { paths = project_test_dirs() }
    elseif t == task.test_type.ALL_DIR_TESTS then
        return { paths = vim.fn.expand("%:p:h") }
    elseif t == task.test_type.FILE_TESTS then
        return { paths = file }
    elseif t == task.test_type.CURRENT_TEST or t == task.test_type.CURRENT_PARAMETRIZED_NUM_TEST then
        local bufnr = vim.api.nvim_get_current_buf()
        local row = vim.api.nvim_win_get_cursor(0)[1] - 1
        local fn = M.test_fn_at_row(bufnr, row)
        if not fn then
            if opts.for_debug then
                vim.notify("No bashunit test_* function under cursor", vim.log.levels.WARN)
                return nil
            end
            vim.notify("No bashunit test_* function under cursor — running file tests", vim.log.levels.WARN)
            return { paths = file }
        end
        return { paths = file, extra = { "--filter", fn } }
    end

    if opts.for_debug then
        vim.notify("Bash debug test type not supported: " .. tostring(t), vim.log.levels.WARN)
        return nil
    end
    vim.notify("Bash test type not supported: " .. tostring(t) .. ", falling back to file", vim.log.levels.WARN)
    return { paths = file }
end

---@param context task.lang.Context
---@return boolean ok, string|nil err
function M.prepare_test_context(context)
    if not M.find_bashunit_bin() then
        return false, "bashunit not found (install or place at <project>/lib/bashunit)"
    end
    return true
end

---@param context task.lang.Context
---@return task.lang.test.TestCmd
function M.build_run_test_cmd(context)
    if not M.find_bashunit_bin() then
        return abort_no_bashunit()
    end
    reset_report_file()

    local sel = resolve_test_selection(context, { for_debug = false })
    if not sel then
        return { cmd = { "echo", "No previous bash test to rerun" } }
    end

    state.last = { paths = sel.paths, extra = sel.extra }
    local cmd = build_cmd(sel.paths, sel.extra)
    return { cmd = cmd, report_dir = M.get_test_report_dir() }
end

--------------------------------------------------------------------------------
-- DAP debug

local bash_path = vim.fn.has("mac") == 1 and "/opt/homebrew/bin/bash" or "/bin/bash"

--- Launch bashdb on the bashunit binary, passing the same args we'd give a normal run.
--- Notes:
--- * The mason bash-debug-adapter wraps bashdb. `program` should be the bash
---   script to debug (we use bashunit), `args` go to the script itself.
--- * We point `cwd` at the project root so relative test paths work.
---@param context task.lang.Context
function M.dap_launch_test(context)
    local bin = M.find_bashunit_bin()
    if not bin then
        vim.notify("bashunit not found; cannot debug", vim.log.levels.ERROR)
        return
    end
    -- bashdb does `bash <program> ...` — it needs an absolute file path, not a
    -- name that depends on $PATH resolution inside its sub-shell.
    if bin:sub(1, 1) ~= "/" then
        local abs = vim.fn.exepath(bin)
        if abs ~= "" then
            bin = abs
        end
    end
    reset_report_file()

    local sel = resolve_test_selection(context, { for_debug = true })
    if not sel then
        return
    end

    state.last = { paths = sel.paths, extra = sel.extra, filetype = vim.bo.filetype }

    -- Same arg shape as a normal bashunit run: --log-junit <report> <paths...> [--filter fn]
    local args = { "--log-junit", report_file() }
    if type(sel.paths) == "string" then
        table.insert(args, sel.paths)
    else
        for _, d in ipairs(sel.paths) do
            table.insert(args, d)
        end
    end
    if sel.extra then
        for _, a in ipairs(sel.extra) do
            table.insert(args, a)
        end
    end

    -- Reparse the test report when the debug session ends so signs/diagnostics
    -- get populated even when the run happens via DAP (no overseer task component).
    local dap = require("dap")
    if not dap.adapters["bashdb"] then
        -- adapter is registered via mason-nvim-dap normally; we accept either.
        local ok_mason, mason = pcall(require, "mason-registry")
        if ok_mason and mason.is_installed("bash-debug-adapter") then
            -- nothing to do, mason-nvim-dap will register it on demand
        end
    end

    -- Reparse the test report only when OUR bashunit test session ends.
    -- (Plain `<leader>rd` debug-current-file also uses bashdb; we must not
    -- trigger test-report load for those non-test sessions.)
    if not dap.listeners.after.event_terminated["bash-test-report-reload"] then
        dap.listeners.after.event_terminated["bash-test-report-reload"] = function(session, _body)
            local cfg = session and session.config
            if not cfg or cfg.name ~= "Bashunit: Debug test" then
                return
            end
            vim.schedule(function()
                pcall(function()
                    require("modules.bash.test-report")
                    -- MERGE the rerun result into the accumulated report (do NOT
                    -- clear): a single-test debug only re-runs one test, so
                    -- clearing would drop every other test's result from the
                    -- tree. Mirrors the non-debug run path (process, no clear).
                    require("modules.common.test-report").process(
                        M.get_test_report_dir(),
                        (state.last and state.last.filetype) or "sh"
                    )
                end)
            end)
        end
    end

    dap.run({
        type = "bashdb",
        request = "launch",
        name = "Bashunit: Debug test",
        showDebugOutput = true,
        pathBashdb = BASH_DEBUG_ADAPTER_PATH .. "/bashdb",
        pathBashdbLib = BASH_DEBUG_ADAPTER_PATH,
        trace = true,
        file = bin,
        program = bin,
        cwd = M.project_root(),
        pathCat = "cat",
        pathBash = bash_path,
        pathMkfifo = "mkfifo",
        pathPkill = "pkill",
        args = args,
        -- Adapter's bashDebug.js interpolates ${args.argsString} into the
        -- final shell cmd; nil renders as the literal string "undefined".
        -- The VSCode extension wrapper defaults it to "" — we must too.
        argsString = "",
        env = {},
        terminalKind = "integrated",
    })
end

return M
