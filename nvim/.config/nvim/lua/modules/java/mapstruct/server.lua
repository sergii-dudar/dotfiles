-- Server Lifecycle Manager for MapStruct IPC
-- Handles starting, stopping, and monitoring the Java server

local config = require("modules.java.mapstruct.config")
local ipc_client = require("modules.java.mapstruct.ipc_client")
local classpath_util = require("utils.java.jdtls-classpath-util")
local logging_util = require("utils.logging-util")
local log = logging_util.new({ name = "MapStruct.Server", filename = "mapstruct-source.log" })

local M = {}

local uv = vim.uv or vim.loop

--- Set log level for this module.
---@param level string|number
function M.set_log_level(level)
    log.set_level(level)
end

-- State
local state = {
    server_job_id = nil,
    socket_path = nil,
    jar_path = nil,
    is_starting = false,
    opts = nil, -- Store original options for restart
}

--- Generate a unique socket path for this Neovim instance.
---@param opts table
---@return string
local function generate_socket_path(opts)
    local tmpdir = vim.fn.expand(opts.socket_dir or config.defaults.socket_dir)
    tmpdir = tmpdir:gsub("/+$", "")
    local nvim_pid = vim.fn.getpid()
    return string.format("%s/mapstruct-ipc-%d.sock", tmpdir, nvim_pid)
end

--- Check whether a filesystem path exists.
---@param path string|nil
---@return boolean
local function path_exists(path)
    return path ~= nil and uv.fs_stat(path) ~= nil
end

--- Filter out SLF4J providers from classpath to avoid conflicts.
---@param classpath string|nil
---@return string|nil
local function filter_slf4j_providers(classpath)
    if not classpath or classpath == "" then
        return classpath
    end

    local exclude_patterns = {
        "logback%-core",
        "logback%-classic",
        "log4j%-slf4j",
        "log4j%-to%-slf4j",
        "slf4j%-simple",
        "slf4j%-jdk14",
        "slf4j%-log4j",
        "slf4j%-reload4j",
        "slf4j%-jcl",
        "slf4j%-nop",
    }

    local entries = vim.split(classpath, ":", { plain = true })
    local filtered = {}

    for _, entry in ipairs(entries) do
        local should_exclude = false
        for _, pattern in ipairs(exclude_patterns) do
            if entry:match(pattern) then
                log.debug("Excluding SLF4J provider from classpath:", entry)
                should_exclude = true
                break
            end
        end
        if not should_exclude then
            table.insert(filtered, entry)
        end
    end

    local filtered_count = #entries - #filtered
    if filtered_count > 0 then
        log.info("Filtered out", filtered_count, "SLF4J provider(s) from classpath")
    end

    return table.concat(filtered, ":")
end

--- Start the Java IPC server.
---@param jar_path string
---@param opts? table
---@param callback? fun(success: boolean, result_or_err?: string)
function M.start(jar_path, opts, callback)
    opts = config.merge(opts)

    if state.server_job_id then
        log.warn("Server already running")
        vim.notify("[MapStruct] Server already running", vim.log.levels.WARN)
        if callback then
            callback(true, state.socket_path)
        end
        return
    end

    if state.is_starting then
        log.warn("Server is already starting")
        vim.notify("[MapStruct] Server is already starting", vim.log.levels.WARN)
        if callback then
            callback(false, "Server is already starting")
        end
        return
    end

    state.is_starting = true
    state.jar_path = jar_path
    state.socket_path = generate_socket_path(opts)
    state.opts = opts -- Store options for restart

    if path_exists(state.socket_path) then
        log.debug("Deleting stale socket before start:", state.socket_path)
        vim.fn.delete(state.socket_path)
    end

    -- Build classpath
    local classpath = jar_path

    -- Try to get classpath from jdtls if enabled (default: true)
    if opts.use_jdtls_classpath ~= false then
        local jdtls_cp = classpath_util.get_classpath({ bufnr = vim.api.nvim_get_current_buf() })
        if jdtls_cp then
            -- Filter out SLF4J providers to avoid conflicts
            jdtls_cp = filter_slf4j_providers(jdtls_cp)
            classpath = classpath .. ":" .. jdtls_cp
            log.info("Using classpath from classpath-util")
        elseif opts.classpath then
            classpath = classpath .. ":" .. opts.classpath
        end
    elseif opts.classpath then
        classpath = classpath .. ":" .. opts.classpath
    end

    -- Build command
    local cmd = {
        opts.java_cmd,
    }

    -- Add log level system property if specified
    if opts.log_level then
        local level_str = logging_util.level_to_string(opts.log_level)
        table.insert(cmd, "-Dmapstruct.log.level=" .. level_str)
        log.info("Setting Java log level:", level_str)
    end

    -- Add log file path system property if specified
    if opts.log_file then
        local log_path = vim.fn.expand(opts.log_file)
        table.insert(cmd, "-Dmapstruct.log.file=" .. log_path)
        log.info("Setting Java log file:", log_path)
    end

    -- dd(classpath)

    -- Add classpath and main class
    table.insert(cmd, "-cp")
    table.insert(cmd, classpath)
    table.insert(cmd, "com.dsm.mapstruct.IpcServer")
    table.insert(cmd, state.socket_path)

    log.info("Starting server on", state.socket_path)
    log.info("Java command:", opts.java_cmd)
    log.info("Use jdtls classpath:", opts.use_jdtls_classpath)
    log.info("Log level from opts:", opts.log_level or "not set")
    log.debug("Full command:", vim.inspect(cmd))
    vim.notify("[MapStruct] Starting server on " .. state.socket_path, vim.log.levels.INFO)

    -- Start server as background job
    state.server_job_id = vim.fn.jobstart(cmd, {
        on_stdout = function(_, data, _)
            if data and #data > 0 then
                for _, line in ipairs(data) do
                    if line ~= "" then
                        log.debug("Server stdout:", line)
                    end
                end
            end
        end,
        on_stderr = function(_, data, _)
            if data and #data > 0 then
                for _, line in ipairs(data) do
                    if line ~= "" then
                        log.error("Server error:", line)
                        -- vim.notify("[MapStruct Server Error] " .. line, vim.log.levels.ERROR)
                    end
                end
            end
        end,
        on_exit = function(_, exit_code, _)
            log.warn("Server exited with code", exit_code)
            log.warn("Server job_id was:", state.server_job_id)
            log.warn("Socket path was:", state.socket_path)
            vim.notify("[MapStruct] Server exited with code " .. exit_code, vim.log.levels.WARN)
            M.cleanup()
        end,
    })

    if state.server_job_id <= 0 then
        log.error("Failed to start server")
        vim.notify("[MapStruct] Failed to start server", vim.log.levels.ERROR)
        state.server_job_id = nil
        state.is_starting = false
        if callback then
            callback(false, "Failed to start server")
        end
        return
    end

    -- Give server time to start, then connect
    vim.defer_fn(function()
        ipc_client.connect(state.socket_path, function(success, err)
            state.is_starting = false
            if callback then
                if success then
                    callback(true, state.socket_path)
                else
                    callback(false, err)
                end
            end
        end)
    end, opts.start_connect_delay_ms)
end

--- Stop the server.
---@param callback? fun(success: boolean)
function M.stop(callback)
    log.info("Stop requested - server_job_id:", state.server_job_id)

    if not state.server_job_id then
        log.info("Server not running, nothing to stop")
        if callback then
            callback(true)
        end
        return
    end

    log.info("Sending shutdown request to server")

    -- Send shutdown request
    if ipc_client.is_connected() then
        log.debug("IPC client is connected, sending shutdown request")
        ipc_client.request("shutdown", {}, function()
            log.debug("Shutdown response received from server")
            -- Server will shut down
        end)
    else
        log.warn("IPC client not connected, cannot send graceful shutdown")
    end

    -- Give server time to shut down gracefully
    vim.defer_fn(function()
        if state.server_job_id then
            log.info("Forcefully stopping job:", state.server_job_id)
            vim.fn.jobstop(state.server_job_id)
        end

        M.cleanup()
        log.info("Server stopped and cleaned up")

        if callback then
            callback(true)
        end
    end, 200)
end

--- Restart the server.
---@param callback? fun(success: boolean, err?: string)
function M.restart(callback)
    log.info("Restarting server...")
    log.info("Current state - jar_path:", state.jar_path)
    log.info("Current state - socket_path:", state.socket_path)
    log.info("Current state - server_job_id:", state.server_job_id)
    log.info("Stored opts:", vim.inspect(state.opts))

    vim.notify("[MapStruct] Restarting server...", vim.log.levels.INFO)
    M.stop(function()
        log.info("Server stopped, waiting before restart")
        vim.defer_fn(function()
            if state.jar_path then
                log.info("Starting server with stored opts")
                -- Reuse stored options instead of empty table
                M.start(state.jar_path, state.opts or {}, callback)
            else
                log.error("Cannot restart - jar_path is nil")
                if callback then
                    callback(false, "jar_path is nil")
                end
            end
        end, 500)
    end)
end

--- Cleanup server resources.
function M.cleanup()
    log.info("Cleanup called")
    log.debug("Disconnecting IPC client")
    ipc_client.disconnect("Server cleanup")

    if state.server_job_id then
        log.debug("Clearing server_job_id:", state.server_job_id)
        state.server_job_id = nil
    end

    -- Clean up socket file
    if path_exists(state.socket_path) then
        log.debug("Deleting socket file:", state.socket_path)
        vim.fn.delete(state.socket_path)
    end

    state.is_starting = false
    log.info("Cleanup completed")
end

--- Check if server is running.
---@return boolean
function M.is_running()
    return state.server_job_id ~= nil
end

--- Get server status.
---@return table
function M.get_status()
    return {
        running = state.server_job_id ~= nil,
        starting = state.is_starting,
        socket_path = state.socket_path,
        jar_path = state.jar_path,
        ipc_status = ipc_client.get_status(),
    }
end

--- Get socket path.
---@return string|nil
function M.get_socket_path()
    return state.socket_path
end

return M
