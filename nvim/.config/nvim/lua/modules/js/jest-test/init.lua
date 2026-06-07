-- JavaScript / TypeScript jest test runner: builds `jest` commands for each
-- task.test_type and launches DAP (pwa-node / js-debug-adapter) for <leader>td.
--
-- REQUIRED:
--   * jest                          (local devDependency: <root>/node_modules/.bin/jest)
--   * node                          (runs jest, and jest.js for test debug)
--   * tree-sitter javascript/typescript parsers (LazyVim default)
--   * js-debug-adapter (Mason)      (only for <leader>td test debug)
--
-- jest emits a machine-readable report via:
--   --json --testLocationInResults --outputFile=<file>
-- which our pipeline (modules.js.test-report) parses for signs / diagnostics /
-- tree view. `--testLocationInResults` gives a `location.line` per test that we
-- use to align report results with treesitter-discovered source positions
-- (this is also how parametrized `test.each` expansions aggregate: every
-- expansion shares the same source line).
--
-- TypeScript projects work through the same path: jest transpiles `.ts` via
-- ts-jest (per the project's jest.config), and `location.line` points at the
-- `.ts` source line.
--
-- Report file: $XDG_CACHE_HOME/nvim/test-report/js/<safe-project-path>/results.json
--
-- Test types:
--   ALL_TESTS / ALL_MODULES_TESTS / SELECTED_MODULES_TESTS -> jest (whole project)
--   ALL_DIR_TESTS                                          -> jest <current_dir>
--   FILE_TESTS                                             -> jest <file>
--   CURRENT_TEST / CURRENT_PARAMETRIZED_NUM_TEST           -> jest <file> -t <pattern>
--   TOGGLE_LAST_DEBUG                                      -> rerun previous command

local M = {}

--------------------------------------------------------------------------------
-- Project root + jest binary resolution

local _root_cache = {}
local _jestbin_cache = {}
local _jestjs_cache = {}

local PROJECT_MARKERS = {
    "jest.config.js",
    "jest.config.ts",
    "jest.config.cjs",
    "jest.config.mjs",
    "jest.config.json",
    "package.json",
    ".git",
}

---@param start string
---@return string|nil
local function find_project_root(start)
    local dir = vim.fn.fnamemodify(start, ":p")
    if vim.fn.isdirectory(dir) == 0 then
        dir = vim.fn.fnamemodify(dir, ":h")
    end
    local prev = nil
    while dir ~= prev and dir ~= "/" and dir ~= "" do
        for _, m in ipairs(PROJECT_MARKERS) do
            if vim.fn.filereadable(dir .. "/" .. m) == 1 or vim.fn.isdirectory(dir .. "/" .. m) == 1 then
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

--- Resolve the jest executable as a command list. Precedence:
---   1. <ancestor>/node_modules/.bin/jest
---   2. `npx jest`
---@return string[]
function M.jest_cmd()
    local root = M.project_root()
    if _jestbin_cache[root] ~= nil then
        return vim.deepcopy(_jestbin_cache[root])
    end

    local dir = root
    local prev = nil
    while dir ~= prev and dir ~= "/" and dir ~= "" do
        local bin = dir .. "/node_modules/.bin/jest"
        if vim.fn.executable(bin) == 1 then
            _jestbin_cache[root] = { bin }
            return { bin }
        end
        prev = dir
        dir = vim.fn.fnamemodify(dir, ":h")
    end

    if vim.fn.executable("npx") == 1 then
        _jestbin_cache[root] = { "npx", "jest" }
        return { "npx", "jest" }
    end

    _jestbin_cache[root] = { "jest" }
    return { "jest" }
end

--- Resolve the jest entry JS file (jest.js) for node-based test debugging.
---@return string|nil
function M.jest_js()
    local root = M.project_root()
    if _jestjs_cache[root] ~= nil then
        return _jestjs_cache[root] or nil
    end
    local dir = root
    local prev = nil
    while dir ~= prev and dir ~= "/" and dir ~= "" do
        local p = dir .. "/node_modules/jest/bin/jest.js"
        if vim.fn.filereadable(p) == 1 then
            _jestjs_cache[root] = p
            return p
        end
        prev = dir
        dir = vim.fn.fnamemodify(dir, ":h")
    end
    _jestjs_cache[root] = false
    return nil
end

---@return boolean
local function jest_available()
    local cmd = M.jest_cmd()
    -- A resolved local binary or npx is assumed runnable.
    if cmd[1] == "jest" then
        return vim.fn.executable("jest") == 1
    end
    return true
end

--------------------------------------------------------------------------------
-- Report location

---@param root string
---@return string
local function report_dir_for(root)
    local safe = root:gsub("[/\\]", "_"):gsub("^_+", "")
    return vim.fn.stdpath("cache") .. "/test-report/js/" .. safe
end

---@return string
function M.get_test_report_dir()
    return report_dir_for(M.project_root())
end

---@return string
local function report_file()
    local dir = M.get_test_report_dir()
    vim.fn.mkdir(dir, "p")
    return dir .. "/results.json"
end

local function reset_report_file()
    local f = report_file()
    local h = io.open(f, "w")
    if h then
        h:close()
    end
end

--- Record whether the upcoming run is name-pattern filtered (single test).
--- The report parser uses this to drop jest's `pending` placeholders that a
--- `--testNamePattern` run emits for non-matching tests (otherwise siblings
--- would be wrongly marked skipped). For full/file/dir runs we keep `pending`
--- so deliberately `test.skip`-ed tests still show a skipped marker.
---@param filtered boolean
local function write_run_meta(filtered)
    local dir = M.get_test_report_dir()
    vim.fn.mkdir(dir, "p")
    local h = io.open(dir .. "/.run-meta", "w")
    if h then
        h:write(filtered and "filtered" or "full")
        h:close()
    end
end

--------------------------------------------------------------------------------
-- Treesitter: build a jest `--testNamePattern` for the test under the cursor.

local TEST_IDENTS = { test = true, it = true, fit = true, xit = true }
local NAMESPACE_IDENTS = { describe = true, fdescribe = true, xdescribe = true }

--- Resolve the leftmost (root) identifier of a call's callee.
--- Handles `test`, `test.only`, `test.each([...])`, `it.concurrent.each`, etc.
---@param node TSNode callee node (the `function` field of a call_expression)
---@return string|nil
local function callee_root_ident(node)
    local t = node:type()
    if t == "identifier" then
        return vim.treesitter.get_node_text(node, 0)
    elseif t == "member_expression" then
        local obj = node:field("object")[1]
        return obj and callee_root_ident(obj) or nil
    elseif t == "call_expression" then
        local fn = node:field("function")[1]
        return fn and callee_root_ident(fn) or nil
    end
    return nil
end

--- Extract the string value of the first argument of a test/describe call.
---@param call TSNode call_expression
---@param bufnr integer
---@return string|nil text, TSNode|nil name_node
local function call_name(call, bufnr)
    local args = call:field("arguments")[1]
    if not args then
        return nil, nil
    end
    for arg in args:iter_children() do
        if arg:named() then
            local at = arg:type()
            if at == "comment" then
                -- skip leading comments
            elseif at == "string" then
                local inner = arg:named_child(0)
                local s = inner and vim.treesitter.get_node_text(inner, bufnr)
                    or vim.treesitter.get_node_text(arg, bufnr):gsub("^[\"']", ""):gsub("[\"']$", "")
                return s, arg
            elseif at == "template_string" then
                local s = vim.treesitter.get_node_text(arg, bufnr):gsub("^`", ""):gsub("`$", "")
                return s, arg
            else
                -- First real argument is not a name string (e.g. the array of a
                -- `.each` inner call); not the name-bearing call.
                return nil, nil
            end
        end
    end
    return nil, nil
end

--- Escape a literal test name for use in a jest testNamePattern regex, then
--- replace jest parameter placeholders (%s, %i, $name, ...) with `.*`.
---@param s string
---@return string
local function to_name_pattern(s)
    local escaped = s:gsub("%(", "\\(")
        :gsub("%)", "\\)")
        :gsub("%]", "\\]")
        :gsub("%[", "\\[")
        :gsub("%*", "\\*")
        :gsub("%+", "\\+")
        :gsub("%-", "\\-")
        :gsub("%?", "\\?")
        :gsub("%$", "\\$")
        :gsub("%^", "\\^")
        :gsub("%/", "\\/")
        :gsub("%.", "\\.")
    -- Template-literal substitutions `${...}` (e.g. `${a} + ${b}`) -> `.*`.
    escaped = escaped:gsub("\\%$%b{}", ".*")
    -- Named params: \$name or \$name.field (dollar was escaped above).
    escaped = escaped:gsub("\\%$[%a_][%w_%.]*", ".*")
    -- Positional placeholders: %s %d %i %f %j %o %p %# %%
    for _, p in ipairs({ "%%p", "%%s", "%%d", "%%i", "%%f", "%%j", "%%o", "%%#", "%%%%" }) do
        escaped = escaped:gsub(p, ".*")
    end
    return escaped
end

---@param bufnr integer
---@return string|nil lang
local function ts_lang_for(bufnr)
    local ft = vim.bo[bufnr].filetype
    if ft == "typescript" then
        return "typescript"
    elseif ft == "typescriptreact" then
        return "tsx"
    elseif ft == "javascriptreact" or ft == "javascript" then
        return "javascript"
    end
    return "javascript"
end

--- Build a `^...$` testNamePattern for the test at the given row.
--- Finds the INNERMOST leaf `test`/`it` call whose range contains `row`
--- (column-independent — the tree view places the cursor at column 0, where a
--- naive ancestor walk would miss deeply-nested tests), then prefixes the
--- enclosing describe chain.
---@param bufnr integer
---@param row integer 0-indexed
---@return string|nil pattern
function M.name_pattern_at_row(bufnr, row)
    local lang = ts_lang_for(bufnr)
    local ok, parser = pcall(vim.treesitter.get_parser, bufnr, lang)
    if not ok or not parser then
        return nil
    end
    local tree = parser:parse()[1]
    if not tree then
        return nil
    end

    local best_chain, best_span
    -- Recursive walk carrying the enclosing describe-title chain.
    local function walk(node, chain)
        for child in node:iter_children() do
            local descend_chain = chain
            if child:type() == "call_expression" then
                local fn = child:field("function")[1]
                local ident = fn and callee_root_ident(fn) or nil
                if ident and (TEST_IDENTS[ident] or NAMESPACE_IDENTS[ident]) then
                    local name = call_name(child, bufnr)
                    if name then
                        local sr, _, er = child:range()
                        if TEST_IDENTS[ident] then
                            -- Leaf test: candidate if its range contains the row.
                            if row >= sr and row <= er then
                                local span = er - sr
                                if best_span == nil or span < best_span then
                                    best_span = span
                                    best_chain = {}
                                    for _, c in ipairs(chain) do
                                        best_chain[#best_chain + 1] = c
                                    end
                                    best_chain[#best_chain + 1] = name
                                end
                            end
                        else
                            -- describe namespace: extend the chain for descendants.
                            descend_chain = {}
                            for _, c in ipairs(chain) do
                                descend_chain[#descend_chain + 1] = c
                            end
                            descend_chain[#descend_chain + 1] = name
                        end
                    end
                end
            end
            walk(child, descend_chain)
        end
    end
    walk(tree:root(), {})

    if not best_chain or #best_chain == 0 then
        return nil
    end

    local parts = {}
    for _, n in ipairs(best_chain) do
        table.insert(parts, to_name_pattern(n))
    end
    return "^" .. table.concat(parts, " ") .. "$"
end

--------------------------------------------------------------------------------
-- Command builders

local function abort(reason)
    return { cmd = { "echo", reason } }
end

---@param targets string[]
---@param extra string[]|nil
---@return string[]
local function build_cmd(targets, extra)
    local cmd = M.jest_cmd()
    cmd = vim.deepcopy(cmd)
    table.insert(cmd, "--json")
    table.insert(cmd, "--testLocationInResults")
    table.insert(cmd, "--no-coverage")
    table.insert(cmd, "--forceExit")
    table.insert(cmd, "--outputFile=" .. report_file())
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

local state = { last = nil }

---@param context task.lang.Context
---@return boolean ok, string|nil err
function M.prepare_test_context(context)
    if not jest_available() then
        return false,
            "jest not found — install it as a devDependency (npm i -D jest) or ensure node_modules/.bin/jest exists"
    end
    return true
end

---@param context task.lang.Context
---@return task.lang.test.TestCmd
function M.build_run_test_cmd(context)
    if not jest_available() then
        return abort("jest not found (npm i -D jest)")
    end
    reset_report_file()

    local t = context.test_type
    local file = vim.fn.expand("%:p")
    local root = M.project_root()
    local cmd
    local filtered = false

    if t == task.test_type.TOGGLE_LAST_DEBUG and state.last and state.last.cmd then
        cmd = state.last.cmd
        filtered = state.last.filtered or false
    elseif
        t == task.test_type.ALL_TESTS
        or t == task.test_type.ALL_MODULES_TESTS
        or t == task.test_type.SELECTED_MODULES_TESTS
    then
        cmd = build_cmd({})
    elseif t == task.test_type.ALL_DIR_TESTS then
        cmd = build_cmd({ vim.fn.expand("%:p:h") })
    elseif t == task.test_type.FILE_TESTS then
        cmd = build_cmd({ file })
    elseif t == task.test_type.CURRENT_TEST or t == task.test_type.CURRENT_PARAMETRIZED_NUM_TEST then
        local bufnr = vim.api.nvim_get_current_buf()
        local row = vim.api.nvim_win_get_cursor(0)[1] - 1
        local pattern = M.name_pattern_at_row(bufnr, row)
        if not pattern then
            vim.notify("No jest test under cursor — running file tests", vim.log.levels.WARN)
            cmd = build_cmd({ file })
        else
            cmd = build_cmd({ file }, { "--testNamePattern=" .. pattern })
            filtered = true
        end
    else
        vim.notify("JS test type not supported: " .. tostring(t) .. ", falling back to file", vim.log.levels.WARN)
        cmd = build_cmd({ file })
    end

    write_run_meta(filtered)
    state.last = { cmd = cmd, filtered = filtered }
    return { cmd = cmd, report_dir = M.get_test_report_dir(), cwd = root }
end

--------------------------------------------------------------------------------
-- DAP debug (node + jest.js via pwa-node / js-debug-adapter).

---@param context task.lang.Context
function M.dap_launch_test(context)
    if not jest_available() then
        vim.notify("jest not found; cannot debug", vim.log.levels.ERROR)
        return
    end
    local jest_js = M.jest_js()
    if not jest_js then
        vim.notify(
            "jest.js entry not found under node_modules/jest/bin — install jest locally to debug tests",
            vim.log.levels.ERROR
        )
        return
    end
    reset_report_file()

    local t = context.test_type
    local file = vim.fn.expand("%:p")
    local root = M.project_root()
    local args
    local filtered = false

    local base = {
        "--runInBand",
        "--no-coverage",
        "--json",
        "--testLocationInResults",
        "--outputFile=" .. report_file(),
    }

    if t == task.test_type.TOGGLE_LAST_DEBUG and state.last and state.last.dap_args then
        args = state.last.dap_args
        filtered = state.last.filtered or false
    elseif t == task.test_type.CURRENT_TEST or t == task.test_type.CURRENT_PARAMETRIZED_NUM_TEST then
        local bufnr = vim.api.nvim_get_current_buf()
        local row = vim.api.nvim_win_get_cursor(0)[1] - 1
        local pattern = M.name_pattern_at_row(bufnr, row)
        if not pattern then
            vim.notify("No jest test under cursor", vim.log.levels.WARN)
            return
        end
        args = vim.deepcopy(base)
        table.insert(args, "--testNamePattern=" .. pattern)
        table.insert(args, file)
        filtered = true
    elseif t == task.test_type.FILE_TESTS then
        args = vim.deepcopy(base)
        table.insert(args, file)
    elseif t == task.test_type.ALL_DIR_TESTS then
        args = vim.deepcopy(base)
        table.insert(args, vim.fn.expand("%:p:h"))
    elseif
        t == task.test_type.ALL_TESTS
        or t == task.test_type.ALL_MODULES_TESTS
        or t == task.test_type.SELECTED_MODULES_TESTS
    then
        args = vim.deepcopy(base)
    else
        vim.notify("JS debug test type not supported: " .. tostring(t), vim.log.levels.WARN)
        return
    end

    state.last = state.last or {}
    state.last.dap_args = args
    state.last.filtered = filtered
    state.last.filetype = vim.bo.filetype
    write_run_meta(filtered)

    local dap = require("dap")

    -- Reload the report only when OUR jest test session ends (plain `<leader>rd`
    -- debug-current-file also uses pwa-node; don't reload for those).
    if not dap.listeners.after.event_terminated["js-test-report-reload"] then
        dap.listeners.after.event_terminated["js-test-report-reload"] = function(session, _body)
            local cfg = session and session.config
            if not cfg or cfg.name ~= "Jest: Debug test" then
                return
            end
            vim.schedule(function()
                pcall(function()
                    require("modules.js.test-report")
                    -- MERGE the rerun result into the accumulated report (do NOT
                    -- clear): a single-test debug only re-runs one test, so
                    -- clearing would drop every other test's result (and its
                    -- name) from the tree. This mirrors the non-debug run path
                    -- (overseer jest_report component -> process, no clear).
                    local core = require("modules.common.test-report")
                    local report_dir = M.get_test_report_dir()
                    core.process(report_dir, (state.last and state.last.filetype) or "javascript")
                end)
            end)
        end
    end

    dap.run({
        type = "pwa-node",
        request = "launch",
        name = "Jest: Debug test",
        runtimeExecutable = "node",
        program = jest_js,
        args = args,
        cwd = root,
        console = "integratedTerminal",
        internalConsoleOptions = "neverOpen",
        sourceMaps = true,
        skipFiles = { "<node_internals>/**", "**/node_modules/**" },
        resolveSourceMapLocations = { root .. "/**", "!**/node_modules/**" },
    })
end

return M
