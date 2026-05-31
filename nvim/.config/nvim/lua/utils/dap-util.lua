-- DAP debugging helpers: export console output, evaluate expressions to file, reset state.
--
-- • show_logs — open DAP log file in split
-- • reset — clear DAP breakpoints and close UI
-- • eval_to_file — evaluate expression and write result to temp file
-- • selection_eval_to_file — evaluate visual selection to file
-- • selection_to_file — write visual selection directly to file

local M = {}

local log_bufnr = nil

---Read dapui console buffer lines. Returns nil if empty/invalid.
---@return string[]|nil
local function read_console()
    local ok_dapui, dapui = pcall(require, "dapui")
    if not ok_dapui then
        return nil
    end

    local console_buf = dapui.elements.console.buffer()
    if not console_buf or not vim.api.nvim_buf_is_valid(console_buf) then
        return nil
    end

    local lines = vim.api.nvim_buf_get_lines(console_buf, 0, -1, false)
    if #lines == 0 or (#lines == 1 and lines[1] == "") then
        return nil
    end

    return lines
end

---Close the log split if open.
local function close_log_win()
    if not log_bufnr then
        return
    end
    if vim.api.nvim_buf_is_valid(log_bufnr) then
        for _, win in ipairs(vim.api.nvim_list_wins()) do
            if vim.api.nvim_win_get_buf(win) == log_bufnr then
                vim.api.nvim_win_close(win, true)
                break
            end
        end
        -- bufhidden=wipe may have already deleted it after window close
        if vim.api.nvim_buf_is_valid(log_bufnr) then
            vim.api.nvim_buf_delete(log_bufnr, { force = true })
        end
    end
    log_bufnr = nil
end

---Show the dapui console output in a bottom split.
---Safe to call multiple times — replaces previous log split.
function M.show_logs()
    local lines = read_console()
    if not lines then
        vim.notify("DAP console is empty or not initialized", vim.log.levels.WARN)
        return
    end

    close_log_win()

    log_bufnr = vim.api.nvim_create_buf(false, true)
    vim.api.nvim_buf_set_lines(log_bufnr, 0, -1, false, lines)

    -- local prev_win = vim.api.nvim_get_current_win()
    require("utils.buffer-util").open_scratch_split(log_bufnr, { max_height = 10 })
    -- vim.cmd("normal! G")
    -- and back
    -- if prev_win and vim.api.nvim_win_is_valid(prev_win) then
    --     vim.api.nvim_set_current_win(prev_win)
    -- end
end

---Close any existing log split. Call on new debug session start.
function M.reset()
    close_log_win()
end

---Evaluate expression via DAP and return result lines via callback.
---@param expr string
---@param callback fun(lines: string[])
local function dap_eval(expr, callback)
    local dap = require("dap")
    local session = dap.session()
    if not session then
        vim.notify("No active DAP session", vim.log.levels.WARN)
        return
    end

    -- Context selection per adapter:
    -- • codelldb (Rust/C/C++): REPL context routes to LLDB's command interpreter,
    --   so bare identifiers fail (`'foo' is not a valid command`). Use `watch`
    --   to go through codelldb's expression evaluator.
    -- • Other adapters (jdtls/java, debugpy/python, ...): `repl` is more flexible
    --   (allows statements, side-effecting method calls, language-specific helpers).
    local context = "repl"
    local adapter_type = session.config and session.config.type
    if adapter_type == "codelldb" or adapter_type == "lldb" then
        context = "watch"
    end

    session:request("evaluate", {
        expression = expr,
        frameId = session.current_frame and session.current_frame.id,
        context = context,
    }, function(err, resp)
        if err then
            vim.schedule(function()
                vim.notify("DAP eval error: " .. tostring(err.message or err), vim.log.levels.ERROR)
            end)
            return
        end
        local result = resp and resp.result
        if type(result) ~= "string" then
            vim.schedule(function()
                vim.notify("DAP eval returned no result", vim.log.levels.WARN)
            end)
            return
        end
        -- Strip outer quotes and unescape (Java String / JSON-style escapes).
        -- Order matters: \\ must be replaced LAST so prior passes don't
        -- re-interpret backslashes produced by it.
        if result:match('^".*"$') then
            result = result:sub(2, -2)
            result = result:gsub('\\"', '"'):gsub("\\n", "\n"):gsub("\\t", "\t"):gsub("\\\\", "\\")
        end
        vim.schedule(function()
            callback(vim.split(result, "\n", { plain = true }))
        end)
    end)
end

---Prompt for expression, evaluate via DAP, return result lines via callback.
---@param callback fun(lines: string[])
local function prompt_eval_dap(callback)
    Snacks.input.input({ prompt = "Expression: " }, function(expr)
        if not expr or expr == "" then
            return
        end
        dap_eval(expr, callback)
    end)
end

---Get current visual selection lines. Works because lazy.nvim's `mode="x"`
---keybind callbacks fire while still in visual mode (via `<cmd>` mapping),
---so a plain `"zy` yanks the active selection.
---@return string[]
local function get_visual_selection()
    local save = vim.fn.getreg("z")
    local save_type = vim.fn.getregtype("z")
    pcall(vim.cmd, 'noautocmd silent normal! "zy')
    local text = vim.fn.getreg("z")
    vim.fn.setreg("z", save, save_type)

    if text == "" then
        return {}
    end

    local lines = vim.split(text, "\n", { plain = true })
    if #lines > 0 and lines[#lines] == "" then
        table.remove(lines)
    end
    return lines
end

---Write lines to path, creating parent dirs.
---@param lines string[]
---@param path string
local function write_lines(lines, path)
    vim.fn.mkdir(vim.fn.fnamemodify(path, ":h"), "p")
    vim.fn.writefile(lines, path)
    vim.notify("Written to " .. path, vim.log.levels.INFO)
end

---Explorer picker to select/create a file, then write lines to it.
---Rooted at current module's src/test/resources.
---Use `a` to create new files, `<CR>` on a file to write to it.
---@param lines string[]
local function pick_and_write(lines)
    local java_util = require("utils.java.java-common")
    local module_root = java_util.get_buffer_project_path() or vim.fn.getcwd()
    -- local resource_dirs = vim.fn.globpath(module_root, "src/**/resources", false, true)
    local resources_dir = module_root .. "/src/test/resources"
    if vim.fn.isdirectory(resources_dir) == 0 then
        resources_dir = module_root .. "/src"
    end

    Snacks.picker.explorer({
        cwd = resources_dir,
        auto_close = true,
        follow_file = false,
        focus = "list",
        actions = {
            write_to_file = function(picker, item)
                if not item then
                    return
                end
                if item.dir then
                    local Tree = require("snacks.explorer.tree")
                    Tree:toggle(item.file)
                    require("snacks.explorer.actions").update(picker, { refresh = true })
                    return
                end
                picker:close()
                vim.notify(item.file)
                write_lines(lines, item.file)
            end,
        },
        win = {
            input = {
                keys = {
                    ["<CR>"] = { "write_to_file", mode = { "i", "n" } },
                },
            },
            list = {
                keys = {
                    ["<CR>"] = "write_to_file",
                    ["l"] = "write_to_file",
                },
            },
        },
    })
end

-- <leader>dwf: DAP eval → pick file/dir → write result
function M.eval_to_file()
    prompt_eval_dap(pick_and_write)
end

function M.selection_eval_to_file()
    local lines = get_visual_selection()
    if #lines == 0 then
        vim.notify("No selection", vim.log.levels.WARN)
        return
    end
    dap_eval(table.concat(lines, "\n"), pick_and_write)
end

-- <leader>dww: write visual selection directly → pick file/dir → write
function M.selection_to_file()
    local lines = get_visual_selection()
    if #lines == 0 then
        vim.notify("No selection", vim.log.levels.WARN)
        return
    end
    pick_and_write(lines)
end

---Open `lines` in a centered floating scratch buffer with wrap enabled.
---Closed with `q` or `<Esc>`. Useful for inspecting long single-line values
---where dapui's hover collapses to a 1-row, unwrapped float.
---@param lines string[]
local function show_in_wrapped_float(lines)
    local buf = vim.api.nvim_create_buf(false, true)
    vim.api.nvim_buf_set_lines(buf, 0, -1, false, lines)
    vim.bo[buf].bufhidden = "wipe"
    vim.bo[buf].filetype = "dap-eval"

    local width = math.min(120, math.floor(vim.o.columns * 0.7))
    -- Compute visual rows needed when wrapped at `width`.
    local needed = 0
    for _, l in ipairs(lines) do
        local w = vim.fn.strdisplaywidth(l)
        needed = needed + math.max(1, math.ceil(w / width))
    end
    local height = math.min(needed, math.floor(vim.o.lines * 0.7))

    local win = vim.api.nvim_open_win(buf, true, {
        relative = "editor",
        row = math.floor((vim.o.lines - height) / 2) - 1,
        col = math.floor((vim.o.columns - width) / 2),
        width = width,
        height = height,
        style = "minimal",
        border = "single",
        title = " DAP eval ",
        title_pos = "center",
    })
    vim.wo[win].wrap = true
    vim.wo[win].linebreak = true
    vim.wo[win].breakindent = true
    vim.wo[win].cursorline = false

    for _, key in ipairs({ "q", "<Esc>" }) do
        vim.keymap.set("n", key, function()
            if vim.api.nvim_win_is_valid(win) then
                vim.api.nvim_win_close(win, true)
            end
        end, { buffer = buf, nowait = true, silent = true })
    end
end

---Evaluate expression under cursor (`<cexpr>`) via DAP, show in wrapped float.
function M.eval_popup()
    local expr = vim.fn.expand("<cexpr>")
    if not expr or expr == "" then
        vim.notify("No expression under cursor", vim.log.levels.WARN)
        return
    end
    dap_eval(expr, show_in_wrapped_float)
end

---Evaluate visual selection via DAP, show result in a wrapped float.
function M.selection_eval_popup()
    local lines = get_visual_selection()
    if #lines == 0 then
        vim.notify("No selection", vim.log.levels.WARN)
        return
    end
    dap_eval(table.concat(lines, "\n"), show_in_wrapped_float)
end

return M
