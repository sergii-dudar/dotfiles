-- External file formatting for buffers that LSP/Conform cannot handle reliably.
--
-- Add future formatters to `formatters_by_filetype`; commands receive the full
-- buffer through stdin and must write the formatted content to stdout.

local M = {}

local notification_title = "External formatter"
local max_error_length = 1000

---@class FileFormatterConfig
---@field command string Executable name.
---@field args string[] Command arguments.

---@type table<string, FileFormatterConfig>
local formatters_by_filetype = {
    json = {
        command = "jq",
        args = { "." },
    },
    xml = {
        command = "xmllint",
        args = { "--format", "-" },
    },
}

---@type table<integer, boolean>
local running_buffers = {}

--- Show a formatter notification.
---@param message string
---@param level integer
local function notify(message, level)
    vim.notify(message, level, { title = notification_title })
end

--- Return configured filetypes in deterministic order.
---@return string[]
local function supported_filetypes()
    local filetypes = {}
    for filetype in pairs(formatters_by_filetype) do
        table.insert(filetypes, filetype)
    end
    table.sort(filetypes)
    return filetypes
end

--- Build argv for a formatter without invoking a shell.
---@param formatter FileFormatterConfig
---@return string[]
local function formatter_command(formatter)
    return vim.list_extend({ formatter.command }, formatter.args)
end

--- Convert formatter stdout to Neovim buffer lines.
---@param output string
---@return string[]
local function output_to_lines(output)
    output = output:gsub("\r\n", "\n")
    if output:sub(-1) == "\n" then
        output = output:sub(1, -2)
    end
    return vim.split(output, "\n", { plain = true })
end

--- Check whether two line lists contain the same text.
---@param left string[]
---@param right string[]
---@return boolean
local function lines_equal(left, right)
    if #left ~= #right then
        return false
    end
    for index, line in ipairs(left) do
        if line ~= right[index] then
            return false
        end
    end
    return true
end

--- Build a bounded error message from a failed formatter result.
---@param result { code?: integer, stdout?: string, stderr?: string }
---@return string
local function formatter_error(result)
    local details = vim.trim(result.stderr or "")
    if details == "" then
        details = vim.trim(result.stdout or "")
    end
    if details == "" then
        details = "exited with code " .. tostring(result.code)
    end
    if #details > max_error_length then
        details = details:sub(1, max_error_length) .. "\n…"
    end
    return details
end

--- Apply successful formatter output if the target buffer is still unchanged.
---@param bufnr integer
---@param changedtick integer
---@param formatter FileFormatterConfig
---@param result { code: integer, stdout?: string, stderr?: string }
local function apply_result(bufnr, changedtick, formatter, result)
    running_buffers[bufnr] = nil

    if not vim.api.nvim_buf_is_valid(bufnr) or not vim.api.nvim_buf_is_loaded(bufnr) then
        return
    end
    if result.code ~= 0 then
        notify(formatter.command .. " failed:\n" .. formatter_error(result), vim.log.levels.ERROR)
        return
    end
    if vim.api.nvim_buf_get_changedtick(bufnr) ~= changedtick then
        notify(
            formatter.command .. " finished, but the buffer changed; formatted output was discarded",
            vim.log.levels.WARN
        )
        return
    end
    if vim.api.nvim_get_option_value("modifiable", { buf = bufnr }) == false then
        notify("Cannot apply " .. formatter.command .. " output: buffer is not modifiable", vim.log.levels.ERROR)
        return
    end

    local output = result.stdout or ""
    if output == "" then
        notify(formatter.command .. " returned no formatted output", vim.log.levels.ERROR)
        return
    end

    local current_lines = vim.api.nvim_buf_get_lines(bufnr, 0, -1, false)
    local formatted_lines = output_to_lines(output)
    if lines_equal(current_lines, formatted_lines) then
        notify("Buffer is already formatted with " .. formatter.command, vim.log.levels.INFO)
        return
    end

    local ok, error_message = pcall(vim.api.nvim_buf_set_lines, bufnr, 0, -1, false, formatted_lines)
    if not ok then
        notify("Cannot apply " .. formatter.command .. " output:\n" .. tostring(error_message), vim.log.levels.ERROR)
        return
    end

    notify("Formatted buffer with " .. formatter.command, vim.log.levels.INFO)
end

--- Format the current buffer with the external formatter registered for its filetype.
function M.format_current_buffer()
    local bufnr = vim.api.nvim_get_current_buf()
    local filetype = vim.api.nvim_get_option_value("filetype", { buf = bufnr })
    local formatter = formatters_by_filetype[filetype]

    if not formatter then
        local displayed_filetype = filetype ~= "" and filetype or "<none>"
        notify(
            string.format(
                "No external formatter configured for filetype '%s' (supported: %s)",
                displayed_filetype,
                table.concat(supported_filetypes(), ", ")
            ),
            vim.log.levels.WARN
        )
        return
    end
    if vim.api.nvim_get_option_value("modifiable", { buf = bufnr }) == false then
        notify("Cannot format: buffer is not modifiable", vim.log.levels.ERROR)
        return
    end
    if vim.fn.executable(formatter.command) ~= 1 then
        notify("External formatter is not executable: " .. formatter.command, vim.log.levels.ERROR)
        return
    end
    if running_buffers[bufnr] then
        notify(formatter.command .. " is already formatting this buffer", vim.log.levels.WARN)
        return
    end

    local content = table.concat(vim.api.nvim_buf_get_lines(bufnr, 0, -1, false), "\n")
    local changedtick = vim.api.nvim_buf_get_changedtick(bufnr)
    local command = formatter_command(formatter)
    running_buffers[bufnr] = true
    notify("Formatting buffer with " .. formatter.command .. "…", vim.log.levels.INFO)

    local ok, system_error = pcall(vim.system, command, { stdin = content, text = true }, function(result)
        vim.schedule(function()
            apply_result(bufnr, changedtick, formatter, result)
        end)
    end)
    if not ok then
        running_buffers[bufnr] = nil
        notify("Could not start " .. formatter.command .. ":\n" .. tostring(system_error), vim.log.levels.ERROR)
    end
end

return M
