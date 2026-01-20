-- Logging utility inspired by tj's vlog.nvim
-- Provides file-based logging with configurable log levels

--  Available levels:
--  - TRACE (0) - Most verbose
--  - DEBUG (1) - Detailed debugging
--  - INFO (2) - General info (default)
--  - WARN (3) - Warnings only
--  - ERROR (4) - Errors only
--  - OFF (5) - No logging

local M = {}

-- Use Neovim's built-in log levels
M.levels = vim.log.levels

-- Convert log level (number or string) to string name
function M.level_to_string(level)
    if type(level) == "string" then
        return string.upper(level)
    elseif type(level) == "number" then
        -- Convert vim.log.levels numeric value to string
        for name, value in pairs(vim.log.levels) do
            if value == level then
                return name
            end
        end
    end
    return "INFO" -- fallback
end

-- Convert log level (string or number) to numeric vim.log.levels value
function M.level_to_number(level)
    if type(level) == "number" then
        return level
    elseif type(level) == "string" then
        local upper = string.upper(level)
        return vim.log.levels[upper] or vim.log.levels.INFO
    end
    return vim.log.levels.INFO -- fallback
end

-- Default configuration
local config = {
    level = vim.log.levels.INFO,
    use_console = false,
    use_file = true,
}

-- Cache for loggers
local loggers = {}

-- Get formatted timestamp
local function get_timestamp()
    return os.date("%Y-%m-%d %H:%M:%S")
end

-- Get level name
local function get_level_name(level)
    for name, value in pairs(M.levels) do
        if value == level then
            return name
        end
    end
    return "UNKNOWN"
end

-- Write to file
local function write_to_file(filepath, msg)
    local file = io.open(filepath, "a")
    if file then
        file:write(msg .. "\n")
        file:close()
    end
end

-- Create a logger for a specific module/component
function M.new(opts)
    opts = opts or {}
    local name = opts.name or "default"
    local filename = opts.filename or "nvim.log"
    local level = opts.level or config.level

    -- Return cached logger if exists
    if loggers[name] then
        return loggers[name]
    end

    local log_dir = vim.fn.stdpath("log")
    local log_path = log_dir .. "/" .. filename

    local logger = {
        name = name,
        filepath = log_path,
        level = level,
    }

    -- Set log level
    function logger.set_level(new_level)
        logger.level = new_level
    end

    -- Get current log level
    function logger.get_level()
        return logger.level
    end

    -- Generic log function
    local function log(level, ...)
        if level < logger.level then
            return
        end

        local parts = { ... }
        local msg_parts = {}

        -- Convert all arguments to strings
        for _, part in ipairs(parts) do
            if type(part) == "table" then
                table.insert(msg_parts, vim.inspect(part))
            else
                table.insert(msg_parts, tostring(part))
            end
        end

        local msg = table.concat(msg_parts, " ")
        local level_name = get_level_name(level)
        local timestamp = get_timestamp()

        local formatted = string.format("[%s] [%s] [%s] %s", timestamp, level_name, logger.name, msg)

        -- Write to file
        if config.use_file then
            vim.schedule(function()
                write_to_file(logger.filepath, formatted)
            end)
        end

        -- Write to console (for debugging the logger itself)
        if config.use_console then
            print(formatted)
        end
    end

    -- Create convenience methods
    logger.trace = function(...)
        log(M.levels.TRACE, ...)
    end

    logger.debug = function(...)
        log(M.levels.DEBUG, ...)
    end

    logger.info = function(...)
        log(M.levels.INFO, ...)
    end

    logger.warn = function(...)
        log(M.levels.WARN, ...)
    end

    logger.error = function(...)
        log(M.levels.ERROR, ...)
    end

    -- Formatted logging methods
    logger.fmt_trace = function(fmt, ...)
        logger.trace(string.format(fmt, ...))
    end

    logger.fmt_debug = function(fmt, ...)
        logger.debug(string.format(fmt, ...))
    end

    logger.fmt_info = function(fmt, ...)
        logger.info(string.format(fmt, ...))
    end

    logger.fmt_warn = function(fmt, ...)
        logger.warn(string.format(fmt, ...))
    end

    logger.fmt_error = function(fmt, ...)
        logger.error(string.format(fmt, ...))
    end

    -- Cache logger
    loggers[name] = logger

    return logger
end

-- Get or create logger
function M.get_logger(name, filename)
    if loggers[name] then
        return loggers[name]
    end
    return M.new({ name = name, filename = filename })
end

-- Clear log file
function M.clear_log(filename)
    local log_dir = vim.fn.stdpath("log")
    local log_path = log_dir .. "/" .. filename
    local file = io.open(log_path, "w")
    if file then
        file:close()
    end
end

-- Set global log level
function M.set_level(level)
    config.level = level
    for _, logger in pairs(loggers) do
        logger.set_level(level)
    end
end

-- Enable/disable console output
function M.use_console(enable)
    config.use_console = enable
end

-- Enable/disable file output
function M.use_file(enable)
    config.use_file = enable
end

return M
