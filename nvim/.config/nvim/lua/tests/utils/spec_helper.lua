local M = {}

_G.unpack = _G.unpack or table.unpack

local function normalize_path(path)
    path = tostring(path or ""):gsub("\\", "/")
    path = path:gsub("/+", "/")
    if #path > 1 then
        path = path:gsub("/$", "")
    end
    return path
end

local function dirname(path)
    path = normalize_path(path)
    return path:match("^(.*)/[^/]*$") or "."
end

local function tail(path)
    return normalize_path(path):match("([^/]+)$") or path
end

local function root_without_extension(path)
    local name = tail(path)
    return name:gsub("%.[^%.]*$", "")
end

local function extension(path)
    local ext = tail(path):match("%.([^%.]*)$")
    return ext or ""
end

local function make_scoped_table(defaults)
    local store = defaults or {}
    return setmetatable(store, {
        __index = function(table_ref, key)
            if type(key) == "number" then
                local value = rawget(table_ref, key)
                if value == nil then
                    value = {}
                    rawset(table_ref, key, value)
                end
                return value
            end
            return rawget(table_ref, key)
        end,
    })
end

local function tbl_extend_force(...)
    local result = {}
    local tables = { ... }
    for index = 2, #tables do
        for key, value in pairs(tables[index] or {}) do
            result[key] = value
        end
    end
    return result
end

local function split_plain(str, delimiter)
    local result = {}
    delimiter = delimiter or "%s"
    if delimiter == "" then
        for index = 1, #str do
            table.insert(result, str:sub(index, index))
        end
        return result
    end

    local start = 1
    while true do
        local found_start, found_end = string.find(str, delimiter, start, true)
        if not found_start then
            table.insert(result, string.sub(str, start))
            break
        end
        table.insert(result, string.sub(str, start, found_start - 1))
        start = found_end + 1
    end
    return result
end

local function is_array(tbl)
    local count = 0
    for key, _ in pairs(tbl) do
        if type(key) ~= "number" or key < 1 or key % 1 ~= 0 then
            return false
        end
        count = count + 1
    end
    for index = 1, count do
        if tbl[index] == nil then
            return false
        end
    end
    return true
end

local function json_escape(str)
    return tostring(str):gsub("\\", "\\\\"):gsub('"', '\\"'):gsub("\n", "\\n"):gsub("\r", "\\r"):gsub("\t", "\\t")
end

local function encode_json(value, nil_sentinel)
    local value_type = type(value)
    if value == nil_sentinel then
        return "null"
    end
    if value_type == "nil" then
        return "null"
    end
    if value_type == "boolean" or value_type == "number" then
        return tostring(value)
    end
    if value_type == "string" then
        return '"' .. json_escape(value) .. '"'
    end
    if value_type ~= "table" then
        return '"' .. json_escape(value) .. '"'
    end

    if is_array(value) then
        local items = {}
        for index = 1, #value do
            table.insert(items, encode_json(value[index], nil_sentinel))
        end
        return "[" .. table.concat(items, ",") .. "]"
    end

    local keys = {}
    for key, _ in pairs(value) do
        table.insert(keys, key)
    end
    table.sort(keys, function(left, right)
        return tostring(left) < tostring(right)
    end)

    local fields = {}
    for _, key in ipairs(keys) do
        table.insert(fields, '"' .. json_escape(key) .. '":' .. encode_json(value[key], nil_sentinel))
    end
    return "{" .. table.concat(fields, ",") .. "}"
end

--- Reset the global Neovim test double to deterministic defaults.
---@return table vim_test_double
---@return table state captured side effects
function M.reset_vim()
    local state = {
        buffers = {},
        loaded_buffers = {},
        buffer_names = {},
        buffer_lines = {},
        buffer_options = {},
        windows = {},
        current_buf = 1,
        current_win = 100,
        current_line = "",
        cursor = { 1, 0 },
        commands = {},
        notifications = {},
        keymaps = {},
        schedules = {},
        system_calls = {},
        writes = {},
        mkdirs = {},
        deleted_paths = {},
        registers = {},
        register_types = {},
        positions = {},
    }

    local cmd = {
        edit = function(target)
            table.insert(state.commands, "edit " .. tostring(target))
        end,
    }
    setmetatable(cmd, {
        __call = function(_, command)
            table.insert(state.commands, command)
        end,
    })

    local vim_test_double = {
        _test_state = state,
        env = {},
        v = { lnum = 1 },
        o = { columns = 120, lines = 40 },
        log = {
            levels = {
                TRACE = 0,
                DEBUG = 1,
                INFO = 2,
                WARN = 3,
                ERROR = 4,
                OFF = 5,
            },
        },
        bo = make_scoped_table({ shiftwidth = 4 }),
        wo = make_scoped_table({}),
        opt_local = {
            indentkeys = {
                appended = {},
                append = function(self, value)
                    table.insert(self.appended, value)
                end,
            },
        },
        cmd = cmd,
    }
    vim_test_double.NIL = {}
    vim_test_double.json = {
        encode = function(value)
            return encode_json(value, vim_test_double.NIL)
        end,
        decode = function()
            error("vim.json.decode is not configured in this test")
        end,
    }

    vim_test_double.loop = {
        os_uname = function()
            return { sysname = "Darwin" }
        end,
        fs_stat = function()
            return nil
        end,
    }
    vim_test_double.uv = vim_test_double.loop

    vim_test_double.fs = {
        joinpath = function(...)
            local parts = { ... }
            return normalize_path(table.concat(parts, "/"))
        end,
        normalize = normalize_path,
    }

    vim_test_double.tbl_extend = function(mode, ...)
        if mode ~= "force" then
            error("test vim.tbl_extend only supports force mode")
        end
        return tbl_extend_force(mode, ...)
    end

    vim_test_double.tbl_filter = function(predicate, list)
        local result = {}
        for _, value in ipairs(list) do
            if predicate(value) then
                table.insert(result, value)
            end
        end
        return result
    end

    vim_test_double.tbl_map = function(mapper, list)
        local result = {}
        for _, value in ipairs(list) do
            table.insert(result, mapper(value))
        end
        return result
    end

    vim_test_double.split = split_plain
    vim_test_double.trim = function(str)
        return tostring(str):match("^%s*(.-)%s*$")
    end
    vim_test_double.startswith = function(str, prefix)
        return tostring(str):sub(1, #prefix) == prefix
    end
    vim_test_double.inspect = function(value)
        if type(value) ~= "table" then
            return tostring(value)
        end
        local parts = {}
        for key, item in pairs(value) do
            table.insert(parts, tostring(key) .. "=" .. tostring(item))
        end
        table.sort(parts)
        return "{" .. table.concat(parts, ", ") .. "}"
    end
    vim_test_double.schedule = function(callback)
        table.insert(state.schedules, callback)
        callback()
    end
    vim_test_double.notify = function(message, level)
        table.insert(state.notifications, { message = message, level = level })
    end
    vim_test_double.uri_to_fname = function(url)
        return tostring(url):gsub("^file://", "")
    end

    vim_test_double.fn = {
        argv = function()
            return {}
        end,
        cindent = function()
            return 0
        end,
        delete = function(path, flags)
            table.insert(state.deleted_paths, { path = path, flags = flags })
            return 0
        end,
        expand = function()
            return ""
        end,
        executable = function()
            return 0
        end,
        filereadable = function()
            return 0
        end,
        fnameescape = function(path)
            return path
        end,
        fnamemodify = function(path, modifier)
            if modifier == ":p" then
                if tostring(path):sub(1, 1) == "/" then
                    return normalize_path(path)
                end
                return normalize_path("/cwd/" .. tostring(path))
            end
            if modifier == ":h" then
                return dirname(path)
            end
            if modifier == ":t" then
                return tail(path)
            end
            if modifier == ":r" then
                return root_without_extension(path)
            end
            if modifier == ":e" then
                return extension(path)
            end
            return path
        end,
        getcwd = function()
            return "/workspace"
        end,
        getline = function()
            return ""
        end,
        getpos = function(mark)
            return state.positions[mark] or { 0, 1, 1, 0 }
        end,
        getreg = function(register)
            return state.registers[register] or ""
        end,
        getregtype = function(register)
            return state.register_types[register] or "v"
        end,
        indent = function()
            return 0
        end,
        isdirectory = function()
            return 0
        end,
        mkdir = function(path, flags)
            table.insert(state.mkdirs, { path = path, flags = flags })
            return 1
        end,
        prevnonblank = function()
            return 0
        end,
        readdir = function()
            return {}
        end,
        reltime = function()
            return { 0, 0 }
        end,
        reltimefloat = function()
            return 0
        end,
        setreg = function(register, value, regtype)
            state.registers[register] = value
            state.register_types[register] = regtype
        end,
        stdpath = function(kind)
            return "/tmp/nvim-test-" .. kind
        end,
        shellescape = function(value)
            return "'" .. tostring(value):gsub("'", "'\\''") .. "'"
        end,
        strdisplaywidth = function(str)
            return #str
        end,
        system = function(command)
            table.insert(state.system_calls, command)
            return ""
        end,
        trim = function(str)
            return tostring(str):match("^%s*(.-)%s*$")
        end,
        writefile = function(lines, path)
            table.insert(state.writes, { lines = lines, path = path })
            return 0
        end,
    }

    vim_test_double.api = {
        nvim_buf_call = function(_, callback)
            return callback()
        end,
        nvim_buf_delete = function(bufnr, opts)
            state.deleted_buffer = { bufnr = bufnr, opts = opts }
        end,
        nvim_buf_get_lines = function(bufnr)
            return state.buffer_lines[bufnr] or {}
        end,
        nvim_buf_get_name = function(bufnr)
            return state.buffer_names[bufnr] or ""
        end,
        nvim_buf_get_text = function(bufnr, start_row, start_col, end_row, end_col)
            local lines = state.buffer_lines[bufnr] or {}
            if start_row == end_row then
                local line = lines[start_row + 1] or ""
                return { line:sub(start_col + 1, end_col) }
            end

            local result = {}
            local first_line = lines[start_row + 1] or ""
            table.insert(result, first_line:sub(start_col + 1))
            for row = start_row + 2, end_row do
                table.insert(result, lines[row] or "")
            end
            local last_line = lines[end_row + 1] or ""
            table.insert(result, last_line:sub(1, end_col))
            return result
        end,
        nvim_buf_is_loaded = function(bufnr)
            return state.loaded_buffers[bufnr] == true
        end,
        nvim_buf_is_valid = function(bufnr)
            return bufnr ~= nil and bufnr ~= false
        end,
        nvim_buf_line_count = function(bufnr)
            return #(state.buffer_lines[bufnr] or {})
        end,
        nvim_buf_set_lines = function(bufnr, _, _, _, lines)
            state.buffer_lines[bufnr] = lines
        end,
        nvim_buf_set_text = function(bufnr, start_row, start_col, end_row, end_col, lines)
            state.set_text = {
                bufnr = bufnr,
                start_row = start_row,
                start_col = start_col,
                end_row = end_row,
                end_col = end_col,
                lines = lines,
            }
        end,
        nvim_create_buf = function()
            state.created_buf = (state.created_buf or 500) + 1
            return state.created_buf
        end,
        nvim_get_current_buf = function()
            return state.current_buf
        end,
        nvim_get_current_line = function()
            return state.current_line
        end,
        nvim_get_current_win = function()
            return state.current_win
        end,
        nvim_get_option_value = function(option, opts)
            local bufnr = opts and opts.buf
            return state.buffer_options[bufnr] and state.buffer_options[bufnr][option]
        end,
        nvim_list_bufs = function()
            return state.buffers
        end,
        nvim_list_wins = function()
            return state.windows
        end,
        nvim_open_win = function(bufnr, enter, opts)
            state.opened_win = { bufnr = bufnr, enter = enter, opts = opts }
            return 900
        end,
        nvim_set_current_win = function(win)
            state.current_win = win
        end,
        nvim_win_close = function(win, force)
            state.closed_win = { win = win, force = force }
        end,
        nvim_win_get_buf = function(win)
            return state.window_buffers and state.window_buffers[win] or state.current_buf
        end,
        nvim_win_get_cursor = function()
            return state.cursor
        end,
        nvim_win_is_valid = function(win)
            return state.valid_windows and state.valid_windows[win] == true
        end,
        nvim_win_set_buf = function(win, bufnr)
            state.window_set_buf = { win = win, bufnr = bufnr }
        end,
        nvim_win_set_height = function(win, height)
            state.window_height = { win = win, height = height }
        end,
    }

    vim_test_double.keymap = {
        set = function(mode, lhs, rhs, opts)
            table.insert(state.keymaps, { mode = mode, lhs = lhs, rhs = rhs, opts = opts })
        end,
    }

    vim_test_double.lsp = {
        get_clients = function()
            return {}
        end,
        get_client_by_id = function()
            return nil
        end,
        buf = {
            code_action = function(opts)
                state.code_action_opts = opts
            end,
        },
        diagnostic = {
            from = function(diagnostics)
                return diagnostics
            end,
        },
        protocol = {
            CodeActionTriggerKind = {
                Invoked = 1,
            },
        },
        util = {
            make_range_params = function()
                return {}
            end,
        },
    }

    vim_test_double.diagnostic = {
        get = function()
            return {}
        end,
    }

    _G.vim = vim_test_double
    _G.Snacks = nil
    _G.LazyVim = nil

    return vim_test_double, state
end

--- Remove modules from package.loaded so each spec can observe module-load behavior.
---@param modules string|string[]
function M.unload(modules)
    if type(modules) == "string" then
        modules = { modules }
    end
    for _, module in ipairs(modules) do
        package.loaded[module] = nil
    end
end

--- Reload a Lua module after clearing it from package.loaded.
---@param module string
---@return any
function M.reload(module)
    M.unload(module)
    return require(module)
end

--- Register a package.loaded test double.
---@param module string
---@param value any
function M.stub_module(module, value)
    package.loaded[module] = value
end

--- Clear package.loaded test doubles.
---@param modules string|string[]
function M.clear_stub_modules(modules)
    M.unload(modules)
end

return M
