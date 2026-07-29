-- External file formatting for buffers that LSP/Conform cannot handle reliably.
--
-- Supported Neovim filetypes and formatters:
-- • json (including *.avsc) — jq
-- • jsonc, graphql, yaml, html — prettierd
-- • javascript, javascriptreact (JSX) — prettierd
-- • typescript, typescriptreact (TSX) — prettierd
-- • xml — xmllint with four-space indentation
--
-- Add future formatters to `formatters_by_filetype`; commands receive the full
-- buffer through stdin and must write the formatted content to stdout.

local M = {}

local notification_title = "External formatter"
local max_error_length = 1000

---@class FileFormatterConfig
---@field command string Executable name.
---@field args string[]|fun(bufnr: integer): string[] Command arguments or a buffer-aware argument builder.
---@field env? table<string, string|number> Environment variables merged into the formatter process.
---@field mason_bin? boolean Prefer the executable symlink from Mason's shared bin directory.

--- Return a real or synthetic filepath that lets prettierd infer the parser.
---@param bufnr integer
---@param fallback_extension string
---@return string
local function prettierd_filepath(bufnr, fallback_extension)
    local filepath = vim.api.nvim_buf_get_name(bufnr)
    if filepath == "" then
        return vim.fs.joinpath(vim.fn.getcwd(), "untitled." .. fallback_extension)
    end
    if vim.fn.fnamemodify(filepath, ":e") == "" then
        return filepath .. "." .. fallback_extension
    end
    return filepath
end

--- Create a prettierd formatter definition for a filetype.
---@param fallback_extension string
---@return FileFormatterConfig
local function prettierd_formatter(fallback_extension)
    return {
        command = "prettierd",
        args = function(bufnr)
            return { prettierd_filepath(bufnr, fallback_extension) }
        end,
        mason_bin = true,
    }
end

---@type table<string, FileFormatterConfig>
local formatters_by_filetype = {
    graphql = prettierd_formatter("graphql"),
    html = prettierd_formatter("html"),
    javascript = prettierd_formatter("js"),
    javascriptreact = prettierd_formatter("jsx"),
    json = {
        command = "jq",
        args = { "." },
    },
    jsonc = prettierd_formatter("jsonc"),
    typescript = prettierd_formatter("ts"),
    typescriptreact = prettierd_formatter("tsx"),
    xml = {
        command = "xmllint",
        args = { "--format", "-" },
        env = {
            XMLLINT_INDENT = string.rep(" ", 4),
        },
    },
    yaml = prettierd_formatter("yaml"),
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

--- Resolve a formatter executable, preferring Mason's shared bin directory.
---@param formatter FileFormatterConfig
---@return string|nil
local function formatter_executable(formatter)
    if formatter.mason_bin then
        local mason_bin_executable = vim.fs.joinpath(vim.fn.stdpath("data"), "mason", "bin", formatter.command)
        if vim.fn.executable(mason_bin_executable) == 1 then
            return mason_bin_executable
        end
    end

    if vim.fn.executable(formatter.command) == 1 then
        return formatter.command
    end
    return nil
end

--- Build argv for a formatter without invoking a shell.
---@param formatter FileFormatterConfig
---@param executable string
---@param bufnr integer
---@return string[]
local function formatter_command(formatter, executable, bufnr)
    local args = type(formatter.args) == "function" and formatter.args(bufnr) or formatter.args
    return vim.list_extend({ executable }, args)
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
    local executable = formatter_executable(formatter)
    if not executable then
        local source = formatter.mason_bin and " (checked Mason bin and PATH)" or ""
        notify("External formatter is not executable: " .. formatter.command .. source, vim.log.levels.ERROR)
        return
    end
    if running_buffers[bufnr] then
        notify(formatter.command .. " is already formatting this buffer", vim.log.levels.WARN)
        return
    end

    local content = table.concat(vim.api.nvim_buf_get_lines(bufnr, 0, -1, false), "\n")
    local changedtick = vim.api.nvim_buf_get_changedtick(bufnr)
    local command = formatter_command(formatter, executable, bufnr)
    running_buffers[bufnr] = true
    notify("Formatting buffer with " .. formatter.command .. "…", vim.log.levels.INFO)

    local system_opts = {
        stdin = content,
        text = true,
        env = formatter.env,
    }
    local ok, system_error = pcall(vim.system, command, system_opts, function(result)
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
