-- Lua / busted test runner: builds `busted` commands for each task.test_type
-- and launches DAP via the local-lua-debugger-vscode adapter (Mason).
--
-- REQUIRED:
--   * busted                      (system, e.g. /usr/bin/busted from luarocks)
--   * local-lua-debugger-vscode   (Mason package) — for <leader>td test debug
--
-- Output: our custom output handler emits NDJSON to:
--   $XDG_CACHE_HOME/nvim/test-report/lua/<safe-project-path>/busted.ndjson
-- which test-report parses for signs / diagnostics / tree view.
--
-- Test types supported:
--   ALL_TESTS / ALL_MODULES_TESTS  -> busted at project root, all spec files
--   ALL_DIR_TESTS                  -> busted on current file's directory
--   FILE_TESTS                     -> busted on the current file
--   CURRENT_TEST                   -> busted on the file + --filter="<exact name>"
--   TOGGLE_LAST_DEBUG              -> rerun previous command (debug or not)
--
-- Test file convention: `*_spec.lua`, `*_test.lua`, or any `.lua` under
-- a `tests/` or `spec/` directory.

local M = {}

local BUSTED_BIN = "/usr/bin/busted"
local OUTPUT_HANDLER = vim.fn.stdpath("config") .. "/lua/modules/lua/busted-test/helpers/output_handler.lua"

--------------------------------------------------------------------------------
-- Project root resolution

local _root_cache = {}

--- Walk up from `start` looking for a busted/luarocks marker.
---@param start string
---@return string|nil
local function find_project_root(start)
    local dir = vim.fn.fnamemodify(start, ":p")
    if vim.fn.isdirectory(dir) == 0 then
        dir = vim.fn.fnamemodify(dir, ":h")
    end
    local markers = { ".busted", ".luarc.json", ".luarc.jsonc", ".git" }
    local rockspec_glob = "*.rockspec"
    local prev = nil
    while dir ~= prev and dir ~= "/" and dir ~= "" do
        for _, m in ipairs(markers) do
            if vim.fn.filereadable(dir .. "/" .. m) == 1 or vim.fn.isdirectory(dir .. "/" .. m) == 1 then
                return dir
            end
        end
        if vim.fn.glob(dir .. "/" .. rockspec_glob) ~= "" then
            return dir
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

---@param root string
---@return string
local function report_dir_for(root)
    local safe = root:gsub("[/\\]", "_"):gsub("^_+", "")
    return vim.fn.stdpath("cache") .. "/test-report/lua/" .. safe
end

---@return string|nil
function M.get_test_report_dir()
    return report_dir_for(M.project_root())
end

--------------------------------------------------------------------------------
-- Sanity checks

local _busted_available
local function busted_available()
    if _busted_available ~= nil then
        return _busted_available
    end
    _busted_available = (vim.fn.executable(BUSTED_BIN) == 1)
    if not _busted_available then
        vim.schedule(function()
            vim.notify(
                "Lua test runner: " .. BUSTED_BIN .. " not found.\n" .. "Install with:  sudo luarocks install busted",
                vim.log.levels.ERROR
            )
        end)
    end
    return _busted_available
end

--------------------------------------------------------------------------------
-- File / test discovery (treesitter)

local lang_adapter = nil
local function get_lang_adapter()
    if not lang_adapter then
        lang_adapter = require("modules.lua.test-report.lang.lua")
    end
    return lang_adapter
end

--- Find the test description chain at cursor (joined describe[s] + it name).
---@return string|nil
local function test_chain_at_cursor()
    local bufnr = vim.api.nvim_get_current_buf()
    local ok, parser = pcall(vim.treesitter.get_parser, bufnr, "lua")
    if not ok or not parser then
        return nil
    end
    local tree = parser:parse()[1]
    if not tree then
        return nil
    end
    local cursor = vim.api.nvim_win_get_cursor(0)
    local row = cursor[1] - 1

    local IT_NAMES = { it = true, pending = true, spec = true, test = true }
    local DESC_NAMES = { describe = true }

    local function call_name(node)
        local fn = node:field("name")[1] or node:child(0)
        if not fn then
            return nil
        end
        return vim.treesitter.get_node_text(fn, bufnr)
    end

    local function first_string_arg(node)
        for child in node:iter_children() do
            local ct = child:type()
            if ct == "arguments" or ct == "argument_list" then
                for sub in child:iter_children() do
                    if sub:type() == "string" then
                        local raw = vim.treesitter.get_node_text(sub, bufnr)
                        if raw:sub(1, 1) == '"' or raw:sub(1, 1) == "'" then
                            return raw:sub(2, -2)
                        end
                        return raw
                    end
                end
            end
        end
        return nil
    end

    local function is_kind(name, set)
        if not name then
            return false
        end
        if set[name] then
            return true
        end
        local base = name:match("^([%w_]+)%.")
        return base ~= nil and set[base]
    end

    -- Find innermost enclosing `it(...)` call
    local node = tree:root():descendant_for_range(row, 0, row, 0)
    local it_node = nil
    while node do
        if node:type() == "function_call" then
            local name = call_name(node)
            if is_kind(name, IT_NAMES) then
                it_node = node
                break
            end
        end
        node = node:parent()
    end
    if not it_node then
        return nil
    end

    local it_desc = first_string_arg(it_node)
    local is_dynamic = it_desc == nil

    -- Walk up collecting enclosing describes. For dynamic `it()` (non-literal
    -- first arg) we fall back to the describe chain and tell the caller to
    -- use a prefix-match filter (no trailing `$`), so the whole describe block
    -- runs. This still lets `<leader>tt` work on parametrized/loop-generated
    -- tests where individual cases have no static name.
    local chain = {}
    if it_desc then
        chain[1] = it_desc
    end
    local p = it_node:parent()
    while p do
        if p:type() == "function_call" then
            local name = call_name(p)
            if is_kind(name, DESC_NAMES) then
                local d = first_string_arg(p)
                if d then
                    table.insert(chain, 1, d)
                end
            end
        end
        p = p:parent()
    end

    if #chain == 0 then
        return nil
    end

    return table.concat(chain, "::"), is_dynamic
end

--------------------------------------------------------------------------------
-- File globbing for ALL_TESTS / ALL_DIR_TESTS

local function is_test_file(path)
    local base = vim.fn.fnamemodify(path, ":t")
    if base:match("_spec%.lua$") or base:match("_test%.lua$") then
        return true
    end
    if path:match("/tests/") or path:match("/spec/") then
        return base:match("%.lua$") ~= nil
    end
    return false
end

---@param dir string
---@return string[]
local function find_test_files(dir)
    local files = vim.fn.systemlist({ "find", dir, "-type", "f", "-name", "*.lua" })
    local result = {}
    for _, f in ipairs(files) do
        if is_test_file(f) then
            table.insert(result, f)
        end
    end
    -- Sort alphabetically to match `busted <dir>` default ordering. Ensures
    -- deterministic runs and matches what users see when running busted
    -- directly on a folder.
    table.sort(result)
    return result
end

--------------------------------------------------------------------------------
-- Command building

--- Build the busted CLI argv (just the args, without bin / shell).
---@param opts { spec_paths: string[]|nil, filter: string|nil, output_file: string, project_root: string }
---@return string[]
local function busted_args(opts)
    local args = {
        "--output=" .. OUTPUT_HANDLER,
        "-Xoutput=" .. opts.output_file,
        "--verbose",
    }
    if opts.filter and opts.filter ~= "" then
        -- busted --filter is a Lua pattern matched against the full path
        -- ("desc1 desc2 it_name"). We escape magic chars for a literal match.
        table.insert(args, "--filter=" .. opts.filter)
    end
    if opts.spec_paths then
        for _, p in ipairs(opts.spec_paths) do
            table.insert(args, p)
        end
    end
    return args
end

--- Escape a string for use as a Lua pattern (busted --filter uses lua patterns).
---@param s string
---@return string
local function lua_pattern_escape(s)
    return (s:gsub("([%.%^%$%(%)%%%+%-%*%?%[%]%{%}|\\])", "%%%1"))
end

--- Convert our "::" desc chain into busted's space-separated path for --filter.
---@param chain string
---@param anchor boolean|nil  if true (default), anchor end with `$` for exact match;
---                            if false, leave open for prefix match (dynamic test names)
---@return string
local function chain_to_filter(chain, anchor)
    if anchor == nil then anchor = true end
    -- busted concatenates descriptions with a single space when matching.
    local parts = vim.split(chain, "::", { plain = true })
    local escaped = {}
    for _, p in ipairs(parts) do
        table.insert(escaped, lua_pattern_escape(p))
    end
    local out = table.concat(escaped, " ")
    if anchor then out = out .. "$" end
    return out
end

--- Build the final shell command (cwd cd, mkdir, rm old report, run, tee).
---@param opts { spec_paths: string[]|nil, filter: string|nil, project_root: string }
---@return string[] cmd, string report_dir
local function build_shell_cmd(opts)
    local report_dir = report_dir_for(opts.project_root)
    local output_file = report_dir .. "/busted.ndjson"

    local args = busted_args({
        spec_paths = opts.spec_paths,
        filter = opts.filter,
        output_file = output_file,
        project_root = opts.project_root,
    })

    local quoted = { vim.fn.shellescape(BUSTED_BIN) }
    for _, a in ipairs(args) do
        table.insert(quoted, vim.fn.shellescape(a))
    end

    local cmd_str = string.format(
        "cd %s && mkdir -p %s && rm -f %s && %s 2>&1 | tee %s; exit ${PIPESTATUS[0]}",
        vim.fn.shellescape(opts.project_root),
        vim.fn.shellescape(report_dir),
        vim.fn.shellescape(output_file),
        table.concat(quoted, " "),
        vim.fn.shellescape(report_dir .. "/busted.out")
    )

    return { "bash", "-c", cmd_str }, report_dir
end

--------------------------------------------------------------------------------
-- Per-test-type resolver

local state = {}

---@param context task.lang.Context
---@return string[]|nil cmd, string|nil report_dir
local function resolve_cmd(context)
    local t = context.test_type
    local root = M.project_root()

    if t == task.test_type.ALL_TESTS or t == task.test_type.ALL_MODULES_TESTS then
        local files = find_test_files(root)
        if #files == 0 then
            vim.notify("Lua test runner: no test files found in " .. root, vim.log.levels.WARN)
            return nil, nil
        end
        state.last = { kind = "all", spec_paths = files, test_type = t }
        return build_shell_cmd({ spec_paths = files, project_root = root })
    end

    if t == task.test_type.ALL_DIR_TESTS then
        local dir = vim.fn.expand("%:p:h")
        local files = find_test_files(dir)
        if #files == 0 then
            vim.notify("Lua test runner: no test files in " .. dir, vim.log.levels.WARN)
            return nil, nil
        end
        state.last = { kind = "dir", spec_paths = files, test_type = t }
        return build_shell_cmd({ spec_paths = files, project_root = root })
    end

    if t == task.test_type.FILE_TESTS then
        local file = vim.fn.expand("%:p")
        if file == "" then
            return nil, nil
        end
        state.last = { kind = "file", spec_paths = { file }, test_type = t }
        return build_shell_cmd({ spec_paths = { file }, project_root = root })
    end

    if t == task.test_type.CURRENT_TEST then
        local file = vim.fn.expand("%:p")
        local chain, is_dynamic = test_chain_at_cursor()
        if file == "" or not chain then
            vim.notify("Lua test runner: no test at cursor", vim.log.levels.WARN)
            return nil, nil
        end
        local filter = chain_to_filter(chain, not is_dynamic)
        state.last = { kind = "single", spec_paths = { file }, filter = filter, chain = chain, test_type = t }
        if is_dynamic then
            vim.notify(
                "Lua test runner: dynamic it() name detected — running enclosing describe block",
                vim.log.levels.INFO
            )
        end
        return build_shell_cmd({ spec_paths = { file }, filter = filter, project_root = root })
    end

    if t == task.test_type.CURRENT_PARAMETRIZED_NUM_TEST then
        vim.notify("Parametrized single-case selection is not implemented for Lua/busted", vim.log.levels.WARN)
        return nil, nil
    end

    if t == task.test_type.TOGGLE_LAST_DEBUG then
        if not state.last then
            vim.notify("No previous lua test to toggle debug for", vim.log.levels.WARN)
            return nil, nil
        end
        return build_shell_cmd({
            spec_paths = state.last.spec_paths,
            filter = state.last.filter,
            project_root = root,
        })
    end

    vim.notify("Unsupported lua test type: " .. tostring(t), vim.log.levels.WARN)
    return nil, nil
end

--------------------------------------------------------------------------------
-- DAP via local-lua-debugger-vscode
--
-- We launch busted under the `local-lua` adapter (mason-installed
-- local-lua-debugger-vscode). That adapter spawns the lua/busted process
-- itself with the debugger pre-loaded, so we just hand it a program +
-- args list and a cwd. osv is NOT usable here: busted runs in a standalone
-- Lua 5.x process, not inside nvim's LuaJIT.

local LUA_LOCAL_EXT = vim.fn.stdpath("data")
    .. "/mason/packages/local-lua-debugger-vscode/extension"
local LUA_LOCAL_DEBUGGER_DIR = LUA_LOCAL_EXT .. "/debugger"

--- The mason-shipped lldebugger.lua uses constructs that are NOT compatible
--- with Lua 5.5 (generic-for loop vars are const-by-default → assignment
--- errors). Probe known locations for a busted installed against Lua ≤ 5.4.
--- Falls back to nil if none found, in which case DAP cannot be used.
---@return string|nil bin, string|nil reason
local function find_dap_busted_bin()
    local home = os.getenv("HOME") or ""
    local candidates = {
        home .. "/.luarocks/bin/busted",
        "/usr/local/bin/busted-5.4",
        "/usr/local/bin/busted-5.3",
        "/usr/local/bin/busted-5.1",
        "/usr/bin/busted-5.4",
        "/usr/bin/busted-5.3",
        "/usr/bin/busted-5.1",
    }
    for _, path in ipairs(candidates) do
        if vim.fn.executable(path) == 1 then
            -- Read shebang/first lines to confirm it does NOT pin Lua 5.5.
            local f = io.open(path, "r")
            if f then
                local head = f:read(4096) or ""
                f:close()
                if not head:find("lua5%.5") and not head:find("/5%.5/") then
                    return path
                end
            end
        end
    end
    return nil, "no busted found that runs on Lua <= 5.4 (lldebugger is incompatible with Lua 5.5)"
end

--- Write (once) a tiny busted --helper that starts lldebugger so breakpoints
--- bind before any spec runs. Cached under nvim cache dir.
---@return string path
local function ensure_start_debug_helper()
    local cache = vim.fn.stdpath("cache")
    vim.fn.mkdir(cache, "p")
    local path = cache .. "/lua-busted-start-debug.lua"
    local body = table.concat({
        "-- Auto-generated by lua/busted-test runner.",
        "-- Loaded by busted via --helper so the debugger is live BEFORE any",
        "-- spec runs and breakpoints can bind.",
        "local ok, dbg = pcall(require, \"lldebugger\")",
        "if not ok then",
        "    io.stderr:write(\"lua-busted dap helper: cannot require lldebugger: \" .. tostring(dbg) .. \"\\n\")",
        "    return",
        "end",
        "dbg.start()",
        "",
    }, "\n")
    local f = io.open(path, "r")
    if f then
        local existing = f:read("*a")
        f:close()
        if existing == body then return path end
    end
    f = assert(io.open(path, "w"))
    f:write(body)
    f:close()
    return path
end

--- Lazily register the `local-lua` adapter (idempotent).
local function ensure_local_lua_adapter()
    local ok, dap = pcall(require, "dap")
    if not ok then
        return false, "nvim-dap not installed"
    end
    if vim.fn.isdirectory(LUA_LOCAL_EXT) == 0 then
        return false,
            "local-lua-debugger-vscode not found at " .. LUA_LOCAL_EXT .. " (install via :Mason)"
    end
    if not dap.adapters["local-lua"] then
        dap.adapters["local-lua"] = {
            type = "executable",
            command = "node",
            args = { LUA_LOCAL_EXT .. "/extension/debugAdapter.js" },
            enrich_config = function(config, on_config)
                if not config["extensionPath"] then
                    config = vim.deepcopy(config)
                    config.extensionPath = LUA_LOCAL_EXT
                end
                on_config(config)
            end,
        }
    end
    return true
end

---@param context task.lang.Context
function M.dap_launch_test(context)
    if not busted_available() then
        return
    end
    local ok_adapter, err = ensure_local_lua_adapter()
    if not ok_adapter then
        vim.notify("Lua test debug: " .. err, vim.log.levels.ERROR)
        return
    end
    local dap_busted_bin, why = find_dap_busted_bin()
    if not dap_busted_bin then
        vim.notify(
            "Lua test debug: " .. why
                .. "\nFix: `luarocks --lua-version=5.4 install --local busted`"
                .. " (or 5.3 / 5.1), then re-run.",
            vim.log.levels.ERROR
        )
        return
    end

    -- Reuse the same context resolution as non-debug to honor the test_type.
    local t = context.test_type
    local root = M.project_root()
    local spec_paths, filter
    if t == task.test_type.CURRENT_TEST then
        local file = vim.fn.expand("%:p")
        local chain, is_dynamic = test_chain_at_cursor()
        if file == "" or not chain then
            vim.notify("Lua test debug: no test at cursor", vim.log.levels.WARN)
            return
        end
        spec_paths = { file }
        filter = chain_to_filter(chain, not is_dynamic)
    elseif t == task.test_type.FILE_TESTS then
        local file = vim.fn.expand("%:p")
        if file == "" then
            return
        end
        spec_paths = { file }
    else
        vim.notify("Lua test debug: only CURRENT_TEST / FILE_TESTS supported", vim.log.levels.WARN)
        return
    end

    local report_dir = report_dir_for(root)
    local output_file = report_dir .. "/busted.ndjson"
    vim.fn.mkdir(report_dir, "p")
    -- truncate previous report
    local f = io.open(output_file, "w")
    if f then
        f:close()
    end

    local helper_path = ensure_start_debug_helper()

    local argv = busted_args({
        spec_paths = spec_paths,
        filter = filter,
        output_file = output_file,
        project_root = root,
    })
    -- Run our debug-loader before specs so breakpoints bind in time.
    table.insert(argv, 1, "--helper=" .. helper_path)

    -- The local-lua adapter spawns via node's child_process with shell:true,
    -- which re-splits args on spaces. Quote any arg containing whitespace so
    -- e.g. `--filter=services integration ...` stays a single argument.
    local quoted_argv = {}
    local SQ = string.char(39) -- single quote
    for _, a in ipairs(argv) do
        if a:find("%s") or a:find(SQ) or a:find([["]]) then
            -- POSIX-safe: wrap in single quotes; escape any embedded single
            -- quotes via the standard close-escape-open trick: ' -> '\''
            local escaped = a:gsub(SQ, SQ .. [[\]] .. SQ .. SQ)
            quoted_argv[#quoted_argv + 1] = SQ .. escaped .. SQ
        else
            quoted_argv[#quoted_argv + 1] = a
        end
    end

    local dap = require("dap")
    dap.run({
        name = "Debug busted (" .. (filter or vim.fn.fnamemodify(spec_paths[1], ":t")) .. ")",
        type = "local-lua",
        request = "launch",
        cwd = root,
        program = { command = dap_busted_bin },
        args = quoted_argv,
    })

    -- Re-parse the report when the debug session ends, so signs/tree update.
    local listener_id = "busted_test_report_" .. tostring(vim.loop.hrtime())
    dap.listeners.after.event_terminated[listener_id] = function()
        dap.listeners.after.event_terminated[listener_id] = nil
        vim.schedule(function()
            pcall(function()
                require("modules.lua.test-report").process({ report_dir }, "lua")
            end)
        end)
    end
end

--------------------------------------------------------------------------------
-- DAP launch for a plain .lua file (no busted). Used by <leader>rd.

--- Probe known Lua interpreters that are NOT 5.5 (so lldebugger works).
---@return string|nil bin, string|nil reason
local function find_dap_lua_bin()
    local candidates = { "lua5.4", "lua5.3", "lua5.1" }
    for _, name in ipairs(candidates) do
        if vim.fn.executable(name) == 1 then
            return name
        end
    end
    return nil, "no lua interpreter found in PATH that is <= 5.4 (lldebugger is incompatible with Lua 5.5)"
end

--- Launch the current buffer's .lua file under DAP using local-lua-debugger-vscode.
--- The adapter's `program.lua + program.file` mode auto-wraps the script with
--- `require("lldebugger").runFile(...)` — no --helper needed.
function M.dap_launch_current_file()
    local ok_adapter, err = ensure_local_lua_adapter()
    if not ok_adapter then
        vim.notify("Lua debug: " .. err, vim.log.levels.ERROR)
        return
    end
    local lua_bin, why = find_dap_lua_bin()
    if not lua_bin then
        vim.notify("Lua debug: " .. why, vim.log.levels.ERROR)
        return
    end
    local file = vim.fn.expand("%:p")
    if file == "" or vim.bo.filetype ~= "lua" then
        vim.notify("Lua debug: not a .lua buffer", vim.log.levels.WARN)
        return
    end
    local cwd = vim.fn.fnamemodify(file, ":h")

    local dap = require("dap")
    dap.run({
        name = "Debug lua (" .. vim.fn.fnamemodify(file, ":t") .. ")",
        type = "local-lua",
        request = "launch",
        cwd = cwd,
        program = { lua = lua_bin, file = file },
        args = {},
    })
end

--------------------------------------------------------------------------------
-- Overseer integration points

---@param context task.lang.Context
---@return boolean ok, string|nil err
function M.prepare_test_context(context)
    -- No interactive prompts needed for lua/busted at the moment.
    return true
end

---@param context task.lang.Context
---@return task.lang.test.TestCmd
function M.build_run_test_cmd(context)
    if not busted_available() then
        return { cmd = { "echo", "busted not installed; aborting." } }
    end
    local cmd, report_dir = resolve_cmd(context)
    if not cmd then
        return { cmd = { "echo", "Could not build lua test command" } }
    end
    return { cmd = cmd, report_dir = report_dir }
end

return M
