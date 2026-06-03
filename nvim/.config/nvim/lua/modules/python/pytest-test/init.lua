-- Python / pytest test runner: builds `pytest` commands for each task.test_type
-- and launches DAP via debugpy (LazyVim's lang.python extra) for <leader>td.
--
-- REQUIRED:
--   * pytest                      (in the project venv or system Python)
--   * debugpy                     (Mason / pip — only for <leader>td test debug)
--   * tree-sitter `python` parser (LazyVim default)
--
-- pytest emits JUnit XML via `--junit-xml=<file>` which our pipeline
-- (modules.python.test-report) parses for signs / diagnostics / tree view.
--
-- unittest projects: we ALWAYS run them via pytest too — pytest natively
-- discovers `unittest.TestCase`. Zero extra deps, one parser, one ID format.
--
-- Report file: $XDG_CACHE_HOME/nvim/test-report/python/<safe-project-path>/junit.xml
--
-- Test types:
--   ALL_TESTS / ALL_MODULES_TESTS / SELECTED_MODULES_TESTS -> pytest <project_root>
--   ALL_DIR_TESTS                                          -> pytest <current_dir>
--   FILE_TESTS                                             -> pytest <file>
--   CURRENT_TEST / CURRENT_PARAMETRIZED_NUM_TEST           -> pytest <file>::<Cls>::<test>
--   TOGGLE_LAST_DEBUG                                      -> rerun previous command

local constants = require("utils.constants")

local M = {}

--------------------------------------------------------------------------------
-- Project root + Python interpreter resolution

local _root_cache = {}
local _python_cache = {}

local PROJECT_MARKERS_FILE = { "pyproject.toml", "setup.py", "setup.cfg", "pytest.ini", "tox.ini", ".git" }
local PROJECT_MARKERS_DIR = { "tests", "test" }

---@param start string
---@return string|nil
local function find_project_root(start)
    local dir = vim.fn.fnamemodify(start, ":p")
    if vim.fn.isdirectory(dir) == 0 then
        dir = vim.fn.fnamemodify(dir, ":h")
    end
    local prev = nil
    while dir ~= prev and dir ~= "/" and dir ~= "" do
        for _, m in ipairs(PROJECT_MARKERS_FILE) do
            if vim.fn.filereadable(dir .. "/" .. m) == 1 or vim.fn.isdirectory(dir .. "/" .. m) == 1 then
                return dir
            end
        end
        for _, m in ipairs(PROJECT_MARKERS_DIR) do
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
    root = root:gsub("/$", "")
    _root_cache[start] = root
    return root
end

--- Resolve a Python interpreter. Precedence:
---   1. `constants.python.python_bin` (manual override)
---   2. $VIRTUAL_ENV/bin/python
---   3. <project>/.venv/bin/python, <project>/venv/bin/python
---   4. `python3` / `python` on $PATH
---@return string|nil
function M.find_python_bin()
    local configured = constants.python and constants.python.python_bin
    if configured and configured ~= "" then
        if vim.fn.executable(configured) == 1 then
            return configured
        end
        vim.notify(
            "python: configured path is not executable: " .. configured .. " — falling back to auto-detect",
            vim.log.levels.WARN
        )
    end

    local root = M.project_root()
    if _python_cache[root] ~= nil then
        return _python_cache[root] or nil
    end

    local venv = vim.env.VIRTUAL_ENV
    if venv and venv ~= "" then
        local p = venv .. "/bin/python"
        if vim.fn.executable(p) == 1 then
            _python_cache[root] = p
            return p
        end
    end

    for _, sub in ipairs({ ".venv", "venv" }) do
        local p = root .. "/" .. sub .. "/bin/python"
        if vim.fn.executable(p) == 1 then
            _python_cache[root] = p
            return p
        end
    end

    for _, name in ipairs({ "python3", "python" }) do
        if vim.fn.executable(name) == 1 then
            local abs = vim.fn.exepath(name)
            local resolved = (abs ~= "" and abs) or name
            _python_cache[root] = resolved
            return resolved
        end
    end

    _python_cache[root] = false
    return nil
end

---@return boolean
local function pytest_available(python_bin)
    local out = vim.fn.systemlist({ python_bin, "-c", "import pytest" })
    return vim.v.shell_error == 0
end

---@param root string
---@return string
local function report_dir_for(root)
    local safe = root:gsub("[/\\]", "_"):gsub("^_+", "")
    return vim.fn.stdpath("cache") .. "/test-report/python/" .. safe
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

local function reset_report_file()
    local f = report_file()
    local h = io.open(f, "w")
    if h then
        h:close()
    end
end

--------------------------------------------------------------------------------
-- Treesitter test discovery (test_fn / TestClass.test_method at cursor)

local _query
local function py_test_query()
    if _query then
        return _query
    end
    _query = vim.treesitter.query.parse(
        "python",
        [[
        ;; Class definitions whose name starts with Test or ends with Test
        (class_definition
          name: (identifier) @cls.name
          (#match? @cls.name "^Test|Test$")
        ) @cls.def

        ;; Function definitions whose name starts with test_
        (function_definition
          name: (identifier) @fn.name
          (#match? @fn.name "^test_?")
        ) @fn.def
    ]]
    )
    return _query
end

--- Find the test under the cursor.
--- Returns { class_name=string|nil, method_name=string, start_row=integer }
--- or nil if no test_* function contains the cursor.
---@param bufnr integer
---@param row integer 0-indexed
---@return table|nil
function M.test_at_row(bufnr, row)
    local ok, parser = pcall(vim.treesitter.get_parser, bufnr, "python")
    if not ok or not parser then
        return nil
    end
    local tree = parser:parse()[1]
    if not tree then
        return nil
    end
    local query = py_test_query()

    -- Collect all function defs containing the cursor (innermost wins by smallest span)
    local fn_match
    local fn_best_span = math.huge
    -- Collect class defs containing the cursor
    local class_match
    local class_best_span = math.huge

    for id, node in query:iter_captures(tree:root(), bufnr) do
        local capture = query.captures[id]
        if capture == "fn.def" then
            local sr, _, er, _ = node:range()
            if row >= sr and row <= er then
                local span = er - sr
                if span < fn_best_span then
                    fn_best_span = span
                    local name_node = node:field("name")[1]
                    fn_match = {
                        name = name_node and vim.treesitter.get_node_text(name_node, bufnr),
                        start_row = sr,
                        node = node,
                    }
                end
            end
        elseif capture == "cls.def" then
            local sr, _, er, _ = node:range()
            if row >= sr and row <= er then
                local span = er - sr
                if span < class_best_span then
                    class_best_span = span
                    local name_node = node:field("name")[1]
                    class_match = {
                        name = name_node and vim.treesitter.get_node_text(name_node, bufnr),
                        start_row = sr,
                    }
                end
            end
        end
    end

    if not fn_match or not fn_match.name then
        return nil
    end
    return {
        class_name = class_match and class_match.name or nil,
        method_name = fn_match.name,
        start_row = fn_match.start_row,
    }
end

--------------------------------------------------------------------------------
-- Command builders

local function abort_no_pytest(reason)
    return { cmd = { "echo", reason } }
end

---@param python_bin string
---@param targets string[]
---@param extra string[]|nil
---@return string[]
local function build_cmd(python_bin, targets, extra)
    local cmd = { python_bin, "-m", "pytest", "--junit-xml=" .. report_file() }
    if extra then
        for _, a in ipairs(extra) do
            table.insert(cmd, a)
        end
    end
    for _, t in ipairs(targets) do
        table.insert(cmd, t)
    end
    return cmd
end

---@param file string  absolute path
---@param at table     { class_name?, method_name }
---@return string      pytest nodeid like "file::Class::method" or "file::method"
local function build_nodeid(file, at)
    if at.class_name then
        return file .. "::" .. at.class_name .. "::" .. at.method_name
    end
    return file .. "::" .. at.method_name
end

--- Memorised last cmd so TOGGLE_LAST_DEBUG can rerun it.
local state = { last = nil }

---@param context task.lang.Context
---@return boolean ok, string|nil err
function M.prepare_test_context(context)
    local py = M.find_python_bin()
    if not py then
        return false, "python interpreter not found"
    end
    if not pytest_available(py) then
        return false, "pytest not importable in " .. py .. " — install with: " .. py .. " -m pip install pytest"
    end
    return true
end

---@param context task.lang.Context
---@return task.lang.test.TestCmd
function M.build_run_test_cmd(context)
    local py = M.find_python_bin()
    if not py then
        return abort_no_pytest("python interpreter not found")
    end
    if not pytest_available(py) then
        return abort_no_pytest("pytest not importable in " .. py .. " (pip install pytest)")
    end
    reset_report_file()

    local t = context.test_type
    local file = vim.fn.expand("%:p")
    local cmd

    if t == task.test_type.TOGGLE_LAST_DEBUG and state.last and state.last.cmd then
        cmd = state.last.cmd
    elseif
        t == task.test_type.ALL_TESTS
        or t == task.test_type.ALL_MODULES_TESTS
        or t == task.test_type.SELECTED_MODULES_TESTS
    then
        cmd = build_cmd(py, { M.project_root() })
    elseif t == task.test_type.ALL_DIR_TESTS then
        cmd = build_cmd(py, { vim.fn.expand("%:p:h") })
    elseif t == task.test_type.FILE_TESTS then
        cmd = build_cmd(py, { file })
    elseif t == task.test_type.CURRENT_TEST or t == task.test_type.CURRENT_PARAMETRIZED_NUM_TEST then
        local bufnr = vim.api.nvim_get_current_buf()
        local row = vim.api.nvim_win_get_cursor(0)[1] - 1
        local at = M.test_at_row(bufnr, row)
        if not at then
            vim.notify("No pytest test_* function under cursor — running file tests", vim.log.levels.WARN)
            cmd = build_cmd(py, { file })
        else
            cmd = build_cmd(py, { build_nodeid(file, at) })
        end
    else
        vim.notify("Python test type not supported: " .. tostring(t) .. ", falling back to file", vim.log.levels.WARN)
        cmd = build_cmd(py, { file })
    end

    state.last = { cmd = cmd }
    return { cmd = cmd, report_dir = M.get_test_report_dir() }
end

--------------------------------------------------------------------------------
-- DAP debug
--
-- Uses the `python` adapter registered by LazyVim's lang.python extra
-- (debugpy). We invoke pytest via `module = "pytest"` and pass the nodeid
-- as the first arg (debugpy requires test target before flags).

---@param context task.lang.Context
function M.dap_launch_test(context)
    local py = M.find_python_bin()
    if not py then
        vim.notify("python interpreter not found; cannot debug", vim.log.levels.ERROR)
        return
    end
    if not pytest_available(py) then
        vim.notify(
            "pytest not importable in " .. py .. " — install with: " .. py .. " -m pip install pytest",
            vim.log.levels.ERROR
        )
        return
    end
    reset_report_file()

    local t = context.test_type
    local file = vim.fn.expand("%:p")
    local args

    if t == task.test_type.TOGGLE_LAST_DEBUG and state.last and state.last.dap_args then
        args = state.last.dap_args
    elseif t == task.test_type.CURRENT_TEST or t == task.test_type.CURRENT_PARAMETRIZED_NUM_TEST then
        local bufnr = vim.api.nvim_get_current_buf()
        local row = vim.api.nvim_win_get_cursor(0)[1] - 1
        local at = M.test_at_row(bufnr, row)
        if not at then
            vim.notify("No pytest test_* function under cursor", vim.log.levels.WARN)
            return
        end
        args = { build_nodeid(file, at), "--junit-xml=" .. report_file() }
    elseif t == task.test_type.FILE_TESTS then
        args = { file, "--junit-xml=" .. report_file() }
    elseif t == task.test_type.ALL_DIR_TESTS then
        args = { vim.fn.expand("%:p:h"), "--junit-xml=" .. report_file() }
    elseif
        t == task.test_type.ALL_TESTS
        or t == task.test_type.ALL_MODULES_TESTS
        or t == task.test_type.SELECTED_MODULES_TESTS
    then
        args = { M.project_root(), "--junit-xml=" .. report_file() }
    else
        vim.notify("Python debug test type not supported: " .. tostring(t), vim.log.levels.WARN)
        return
    end

    state.last = state.last or {}
    state.last.dap_args = args

    local dap = require("dap")

    -- Reparse the test report only when OUR pytest test session ends.
    -- (Plain `<leader>rd` debug-current-file also uses the python adapter;
    -- we must not trigger test-report load for those non-test sessions.)
    if not dap.listeners.after.event_terminated["python-test-report-reload"] then
        dap.listeners.after.event_terminated["python-test-report-reload"] = function(session, _body)
            local cfg = session and session.config
            if not cfg or cfg.name ~= "Pytest: Debug test" then
                return
            end
            vim.schedule(function()
                pcall(function()
                    require("modules.python.test-report")
                    require("modules.common.test-report").load_existing()
                end)
            end)
        end
    end

    dap.run({
        type = "python",
        request = "launch",
        name = "Pytest: Debug test",
        module = "pytest",
        args = args,
        console = "integratedTerminal",
        cwd = M.project_root(),
        justMyCode = false,
        pythonPath = py,
    })
end

return M
