-- IPC Client for MapStruct Path Explorer
-- Handles Unix domain socket communication with the Java server

local log = require("utils.logging-util").new({ name = "MapStruct.IPC", filename = "mapstruct-source.log" })

local M = {}

-- State
local state = {
    socket_fd = nil,
    connected = false,
    pending_requests = {},
    request_id_counter = 0,
    read_buffer = "",
    heartbeat_timer = nil,
}

-- Configuration
local config = {
    socket_path = nil,
    heartbeat_interval_ms = 10000,
    request_timeout_ms = 5000,
}

-- Generate unique request ID
local function generate_request_id()
    state.request_id_counter = state.request_id_counter + 1
    return tostring(state.request_id_counter)
end

-- Handle incoming data from socket
local function handle_data(data)
    state.read_buffer = state.read_buffer .. data

    -- Process complete lines (newline-delimited JSON)
    while true do
        local newline_pos = state.read_buffer:find("\n")
        if not newline_pos then
            break
        end

        local line = state.read_buffer:sub(1, newline_pos - 1)
        state.read_buffer = state.read_buffer:sub(newline_pos + 1)

        if line ~= "" then
            vim.schedule(function()
                handle_response(line)
            end)
        end
    end
end

-- Handle a complete JSON response
function handle_response(json_str)
    local ok, response = pcall(vim.json.decode, json_str)
    if not ok then
        log.error("Failed to decode JSON:", json_str)
        vim.notify("[MapStruct] Failed to decode JSON: " .. json_str, vim.log.levels.ERROR)
        return
    end

    -- Log response only
    if response.error then
        log.info("<<< RESPONSE ERROR: " .. response.error)
    elseif response.result then
        log.info("<<< RESPONSE: " .. vim.inspect(response.result))
    end

    local request_id = response.id
    if request_id and state.pending_requests[request_id] then
        local callback = state.pending_requests[request_id]
        state.pending_requests[request_id] = nil

        if response.error then
            callback(nil, response.error)
        else
            callback(response.result, nil)
        end
    end
end

-- Send heartbeat to keep connection alive
local function send_heartbeat()
    if not state.connected then
        return
    end

    M.request("heartbeat", {}, function(result, err)
        if err then
            log.warn("Heartbeat failed:", err)
            -- Disconnect so the next request will trigger reconnection
            M.disconnect()
            vim.schedule(function()
                vim.notify("[MapStruct] Connection lost - will reconnect on next request", vim.log.levels.WARN)
            end)
        end
    end)
end

-- Start heartbeat timer
local function start_heartbeat()
    if state.heartbeat_timer then
        return
    end

    state.heartbeat_timer = vim.loop.new_timer()
    state.heartbeat_timer:start(
        config.heartbeat_interval_ms,
        config.heartbeat_interval_ms,
        vim.schedule_wrap(function()
            send_heartbeat()
        end)
    )
end

-- Stop heartbeat timer
local function stop_heartbeat()
    if state.heartbeat_timer then
        state.heartbeat_timer:stop()
        state.heartbeat_timer:close()
        state.heartbeat_timer = nil
    end
end

-- Connect to the Unix domain socket
function M.connect(socket_path, callback)
    if state.connected then
        if callback then
            callback(true, nil)
        end
        return true
    end

    config.socket_path = socket_path

    -- Wait for socket file to exist
    local max_attempts = 20
    local attempt = 0
    while attempt < max_attempts do
        if vim.fn.filereadable(socket_path) == 1 then
            break
        end
        attempt = attempt + 1
        vim.cmd("sleep 100m")
    end

    if vim.fn.filereadable(socket_path) ~= 1 then
        local err = "Socket file not found: " .. socket_path
        if callback then
            callback(false, err)
        end
        return false
    end

    -- Connect using Lua socket (requires luv/libuv)
    local uv = vim.loop
    state.socket_fd = uv.new_pipe(false)

    state.socket_fd:connect(socket_path, function(err)
        if err then
            vim.schedule(function()
                log.error("Failed to connect:", err)
                vim.notify("[MapStruct] Failed to connect: " .. err, vim.log.levels.ERROR)
                state.connected = false
                if callback then
                    callback(false, err)
                end
            end)
            return
        end

        vim.schedule(function()
            log.info("Connected to IPC server")
            vim.notify("[MapStruct] Connected to IPC server", vim.log.levels.INFO)
        end)

        state.connected = true

        -- Start reading responses
        state.socket_fd:read_start(function(read_err, data)
            if read_err then
                vim.schedule(function()
                    log.error("Socket read error:", read_err)
                    vim.notify("[MapStruct] Socket read error: " .. read_err, vim.log.levels.ERROR)
                    M.disconnect()
                end)
                return
            end

            if data then
                handle_data(data)
            else
                -- Connection closed
                vim.schedule(function()
                    log.warn("Server disconnected")
                    vim.notify("[MapStruct] Server disconnected", vim.log.levels.WARN)
                    M.disconnect()
                end)
            end
        end)

        -- Start heartbeat timer
        start_heartbeat()

        if callback then
            vim.schedule(function()
                callback(true, nil)
            end)
        end
    end)

    return true
end

-- Send a request to the server
function M.request(method, params, callback)
    if not state.connected then
        if callback then
            callback(nil, "Not connected")
        end
        return
    end

    local request_id = generate_request_id()
    local request = {
        id = request_id,
        method = method,
        params = params or {}
    }

    local json_str = vim.json.encode(request) .. "\n"

    -- Log request params only
    log.info(">>> REQUEST: " .. vim.inspect(params))

    -- Store callback for this request
    if callback then
        state.pending_requests[request_id] = callback

        -- Set timeout for request
        vim.defer_fn(function()
            if state.pending_requests[request_id] then
                state.pending_requests[request_id] = nil
                callback(nil, "timeout")
            end
        end, config.request_timeout_ms)
    end

    -- Send request
    state.socket_fd:write(json_str, function(err)
        if err then
            vim.schedule(function()
                if callback and state.pending_requests[request_id] then
                    state.pending_requests[request_id] = nil
                    callback(nil, err)
                end
            end)
        end
    end)
end

-- Disconnect from server
function M.disconnect()
    stop_heartbeat()

    if state.socket_fd then
        state.socket_fd:close()
        state.socket_fd = nil
    end

    state.connected = false
    state.pending_requests = {}
    state.read_buffer = ""
end

-- Check if connected
function M.is_connected()
    return state.connected
end

-- Get connection status
function M.get_status()
    return {
        connected = state.connected,
        socket_path = config.socket_path,
        pending_requests = vim.tbl_count(state.pending_requests),
    }
end

return M
