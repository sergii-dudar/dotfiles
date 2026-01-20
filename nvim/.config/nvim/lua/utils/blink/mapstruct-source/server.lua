-- Server Lifecycle Manager for MapStruct IPC
-- Handles starting, stopping, and monitoring the Java server

local ipc_client = require("utils.blink.mapstruct-source.ipc_client")
local classpath_util = require("utils.blink.mapstruct-source.classpath-util")
local log = require("utils.logging-util").new({ name = "MapStruct.Server", filename = "mapstruct-source.log" })

local M = {}

-- Set log level for this module
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

-- Generate unique socket path for this Neovim instance
local function generate_socket_path()
    local tmpdir = "/tmp"
    local nvim_pid = vim.fn.getpid()
    return string.format("%s/mapstruct-ipc-%d.sock", tmpdir, nvim_pid)
end

-- Start the Java IPC server
function M.start(jar_path, opts, callback)
    opts = opts or {}

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
        return
    end

    state.is_starting = true
    state.jar_path = jar_path
    state.socket_path = generate_socket_path()
    state.opts = opts -- Store options for restart

    -- Build classpath
    local classpath = jar_path

    -- Try to get classpath from jdtls if enabled (default: true)
    if opts.use_jdtls_classpath ~= false then
        local jdtls_cp = classpath_util.get_classpath({ bufnr = vim.api.nvim_get_current_buf() })
        if jdtls_cp then
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
        opts.java_cmd or "java",
    }

    -- Add log level system property if specified
    if opts.log_level then
        table.insert(cmd, "-Dmapstruct.log.level=" .. opts.log_level)
        log.info("Setting log level:", opts.log_level)
    end

    -- Add classpath and main class
    table.insert(cmd, "-cp")
    table.insert(cmd, classpath)
    table.insert(cmd, "com.dsm.mapstruct.IpcServer")
    table.insert(cmd, state.socket_path)

    log.info("Starting server on", state.socket_path)
    log.info("Java command:", opts.java_cmd or "java")
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
                        vim.notify("[MapStruct Server Error] " .. line, vim.log.levels.ERROR)
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
        state.is_starting = false
        ipc_client.connect(state.socket_path, function(success, err)
            if callback then
                if success then
                    callback(true, state.socket_path)
                else
                    callback(false, err)
                end
            end
        end)
    end, 1000)
end

-- Stop the server
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

-- Restart the server
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

-- Cleanup server resources
function M.cleanup()
    log.info("Cleanup called")
    log.debug("Disconnecting IPC client")
    ipc_client.disconnect()

    if state.server_job_id then
        log.debug("Clearing server_job_id:", state.server_job_id)
        state.server_job_id = nil
    end

    -- Clean up socket file
    if state.socket_path and vim.fn.filereadable(state.socket_path) == 1 then
        log.debug("Deleting socket file:", state.socket_path)
        vim.fn.delete(state.socket_path)
    end

    state.is_starting = false
    log.info("Cleanup completed")
end

-- Check if server is running
function M.is_running()
    return state.server_job_id ~= nil
end

-- Get server status
function M.get_status()
    return {
        running = state.server_job_id ~= nil,
        starting = state.is_starting,
        socket_path = state.socket_path,
        jar_path = state.jar_path,
        ipc_status = ipc_client.get_status(),
    }
end

-- Get socket path
function M.get_socket_path()
    return state.socket_path
end

return M