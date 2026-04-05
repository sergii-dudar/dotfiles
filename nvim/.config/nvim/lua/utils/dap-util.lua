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
        return
    end

    close_log_win()

    log_bufnr = vim.api.nvim_create_buf(false, true)
    vim.api.nvim_buf_set_lines(log_bufnr, 0, -1, false, lines)

    require("utils.buffer-util").open_scratch_split(log_bufnr)
    vim.cmd("normal! G")
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

    session:request("evaluate", {
        expression = expr,
        frameId = session.current_frame and session.current_frame.id,
        context = "repl",
    }, function(err, resp)
        if err then
            vim.schedule(function()
                vim.notify("DAP eval error: " .. tostring(err.message or err), vim.log.levels.ERROR)
            end)
            return
        end
        local result = resp.result
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

---Get visual selection lines.
---@return string[]
local function get_visual_selection()
    local save = vim.fn.getreg("z")
    local save_type = vim.fn.getregtype("z")
    vim.cmd('noautocmd normal! "zy')
    local text = vim.fn.getreg("z")
    vim.fn.setreg("z", save, save_type)

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

---Mixed files+dirs picker. File selected → write directly. Dir selected → prompt for filename.
---Scoped to resources dirs under current module.
---@param lines string[]
local function pick_and_write(lines)
    local java_util = require("utils.java.java-common")
    local module_root = java_util.get_buffer_project_path() or vim.fn.getcwd()
    local Preview = require("snacks.picker.preview")
    Snacks.picker({
        title = "Write to",
        finder = "proc",
        format = "file",
        cmd = "fd",
        args = (function()
            local args = { "--type", "f", "--type", "d", "--hidden", "--exclude", ".git" }
            -- local resource_dirs = vim.fn.globpath(module_root, "src/**/resources", false, true)
            local resource_dirs = { module_root .. "/src/test/resources" }
            if #resource_dirs > 0 then
                for _, dir in ipairs(resource_dirs) do
                    vim.list_extend(args, { "--search-path", dir })
                end
            else
                vim.list_extend(args, { "--search-path", module_root .. "/src" })
            end
            return args
        end)(),
        transform = function(item)
            local path = item.text
            item.file = path
            item.dir = vim.fn.isdirectory(path) == 1
        end,
        preview = function(ctx)
            if ctx.item.dir then
                Preview.cmd({
                    "eza",
                    "--tree",
                    "--icons",
                    "--level=1",
                    "--color=always",
                    "--group-directories-first",
                    ctx.item.text,
                }, ctx)
            else
                Preview.file(ctx)
            end
        end,
        confirm = function(picker, item)
            picker:close()
            if not item then
                return
            end
            if item.dir then
                vim.schedule(function()
                    Snacks.input.input({ prompt = "File name: " }, function(name)
                        if not name or name == "" then
                            return
                        end
                        write_lines(lines, item.text .. "/" .. name)
                    end)
                end)
            else
                write_lines(lines, item.file)
            end
        end,
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

return M
