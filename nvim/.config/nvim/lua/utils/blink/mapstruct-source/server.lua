-- Server Lifecycle Manager for MapStruct IPC
-- Handles starting, stopping, and monitoring the Java server

local ipc_client = require("utils.blink.mapstruct-source.ipc_client")
local classpath_util = require("utils.blink.mapstruct-source.classpath-util")
local log = require("utils.logging-util").new({ name = "MapStruct.Server", filename = "mapstruct-source.log" })

local M = {}

-- State
local state = {
    server_job_id = nil,
    socket_path = nil,
    jar_path = nil,
    is_starting = false,
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
        "-cp",
        classpath,
        "com.dsm.mapstruct.IpcServer",
        state.socket_path,
    }

    log.info("Starting server on", state.socket_path)
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
    if not state.server_job_id then
        if callback then
            callback(true)
        end
        return
    end

    -- Send shutdown request
    if ipc_client.is_connected() then
        ipc_client.request("shutdown", {}, function()
            -- Server will shut down
        end)
    end

    -- Give server time to shut down gracefully
    vim.defer_fn(function()
        if state.server_job_id then
            vim.fn.jobstop(state.server_job_id)
        end

        M.cleanup()

        if callback then
            callback(true)
        end
    end, 200)
end

-- Restart the server
function M.restart(callback)
    log.info("Restarting server...")
    vim.notify("[MapStruct] Restarting server...", vim.log.levels.INFO)
    M.stop(function()
        vim.defer_fn(function()
            if state.jar_path then
                M.start(state.jar_path, {}, callback)
            end
        end, 500)
    end)
end

-- Cleanup server resources
function M.cleanup()
    ipc_client.disconnect()

    if state.server_job_id then
        state.server_job_id = nil
    end

    -- Clean up socket file
    if state.socket_path and vim.fn.filereadable(state.socket_path) == 1 then
        vim.fn.delete(state.socket_path)
    end

    state.is_starting = false
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
