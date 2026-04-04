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

---Prompt for a file path and write lines to it.
---@param lines string[]
local function write_to_file(lines)
    Snacks.input.input({ prompt = "Write to file: ", completion = "file" }, function(path)
        if not path or path == "" then
            return
        end
        path = vim.fn.expand(path)
        vim.fn.mkdir(vim.fn.fnamemodify(path, ":h"), "p")
        vim.fn.writefile(lines, path)
        vim.notify("Written to " .. path, vim.log.levels.INFO)
    end)
end

---Evaluate a DAP expression and write result to file.
function M.eval_to_file()
    local dap = require("dap")
    local session = dap.session()
    if not session then
        vim.notify("No active DAP session", vim.log.levels.WARN)
        return
    end

    Snacks.input.input({ prompt = "Expression: " }, function(expr)
        if not expr or expr == "" then
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
            -- Strip surrounding quotes and unescape Java strings
            if result:match('^".*"$') then
                result = result:sub(2, -2)
                result = result:gsub('\\"', '"'):gsub("\\n", "\n"):gsub("\\t", "\t"):gsub("\\\\", "\\")
            end
            local lines = vim.split(result, "\n", { plain = true })
            vim.schedule(function()
                write_to_file(lines)
            end)
        end)
    end)
end

---Write visual selection to file.
function M.selection_to_file()
    local save = vim.fn.getreg("z")
    local save_type = vim.fn.getregtype("z")
    vim.cmd('noautocmd normal! "zy')
    local text = vim.fn.getreg("z")
    vim.fn.setreg("z", save, save_type)

    local lines = vim.split(text, "\n", { plain = true })
    if #lines > 0 and lines[#lines] == "" then
        table.remove(lines)
    end
    if #lines == 0 then
        vim.notify("No selection", vim.log.levels.WARN)
        return
    end
    write_to_file(lines)
end

return M
