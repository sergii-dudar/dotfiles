-- IPC Client for MapStruct Path Explorer
-- Handles Unix domain socket communication with the Java server

local mapstruct_config = require("modules.java.mapstruct.config")
local log = require("utils.logging-util").new({ name = "MapStruct.IPC", filename = "mapstruct-source.log" })

local M = {}

local uv = vim.uv or vim.loop
local handle_response

--- Set log level for this module.
---@param level string|number
function M.set_log_level(level)
    log.set_level(level)
end

-- State
local state = {
    socket_fd = nil,
    connected = false,
    pending_requests = {},
    request_id_counter = 0,
    read_buffer = "",
    heartbeat_timer = nil,
    connect_token = 0,
}

-- Configuration
local config = {
    socket_path = nil,
    heartbeat_interval_ms = mapstruct_config.defaults.heartbeat_interval_ms,
    request_timeout_ms = mapstruct_config.defaults.request_timeout_ms,
    connect_timeout_ms = mapstruct_config.defaults.connect_timeout_ms,
    connect_poll_interval_ms = mapstruct_config.defaults.connect_poll_interval_ms,
}

--- Generate unique request ID.
---@return string
local function generate_request_id()
    state.request_id_counter = state.request_id_counter + 1
    return tostring(state.request_id_counter)
end

--- Check whether the socket path has appeared.
---@param socket_path string
---@return boolean
local function socket_exists(socket_path)
    return uv.fs_stat(socket_path) ~= nil
end

--- Close the active pipe handle if one exists.
local function close_socket()
    if state.socket_fd then
        pcall(function()
            if not state.socket_fd:is_closing() then
                state.socket_fd:close()
            end
        end)
        state.socket_fd = nil
    end
end

--- Fail and clear all pending IPC callbacks.
---@param reason string
local function fail_pending_requests(reason)
    local pending = state.pending_requests
    state.pending_requests = {}

    for _, callback in pairs(pending) do
        vim.schedule(function()
            callback(nil, reason)
        end)
    end
end

--- Handle incoming data from socket.
---@param data string
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

--- Handle a complete JSON response.
---@param json_str string
handle_response = function(json_str)
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

        callback(response.result, response.error)
    end
end

--- Send heartbeat to keep connection alive.
local function send_heartbeat()
    if not state.connected then
        return
    end

    M.request("heartbeat", {}, function(_, err)
        if err then
            log.warn("Heartbeat failed:", err)
            -- Disconnect so the next request will trigger reconnection
            M.disconnect("Heartbeat failed: " .. err)
            vim.schedule(function()
                vim.notify("[MapStruct] Connection lost - will reconnect on next request", vim.log.levels.WARN)
            end)
        end
    end)
end

--- Start heartbeat timer.
local function start_heartbeat()
    if state.heartbeat_timer then
        return
    end

    state.heartbeat_timer = uv.new_timer()
    state.heartbeat_timer:start(
        config.heartbeat_interval_ms,
        config.heartbeat_interval_ms,
        vim.schedule_wrap(function()
            send_heartbeat()
        end)
    )
end

--- Stop heartbeat timer.
local function stop_heartbeat()
    if state.heartbeat_timer then
        state.heartbeat_timer:stop()
        state.heartbeat_timer:close()
        state.heartbeat_timer = nil
    end
end

--- Update IPC timing options from the MapStruct setup configuration.
---@param opts? table
function M.configure(opts)
    opts = mapstruct_config.merge(opts)

    local heartbeat_changed = config.heartbeat_interval_ms ~= opts.heartbeat_interval_ms

    config.heartbeat_interval_ms = opts.heartbeat_interval_ms
    config.request_timeout_ms = opts.request_timeout_ms
    config.connect_timeout_ms = opts.connect_timeout_ms
    config.connect_poll_interval_ms = opts.connect_poll_interval_ms

    if heartbeat_changed and state.heartbeat_timer then
        stop_heartbeat()
        if state.connected then
            start_heartbeat()
        end
    end
end

--- Connect to the Unix domain socket.
---@param socket_path string
---@param callback? fun(success: boolean, err?: string)
---@return boolean
function M.connect(socket_path, callback)
    if state.connected then
        if callback then
            callback(true, nil)
        end
        return true
    end

    config.socket_path = socket_path
    state.connect_token = state.connect_token + 1
    local connect_token = state.connect_token
    local started_at = uv.now()

    close_socket()
    state.connected = false

    local function finish(success, err)
        if connect_token ~= state.connect_token then
            return
        end

        if callback then
            callback(success, err)
        end
    end

    local function open_pipe()
        if connect_token ~= state.connect_token then
            return
        end

        state.socket_fd = uv.new_pipe(false)
        if not state.socket_fd then
            finish(false, "Failed to allocate IPC pipe")
            return
        end

        state.socket_fd:connect(socket_path, function(err)
            if connect_token ~= state.connect_token then
                return
            end

            if err then
                vim.schedule(function()
                    log.error("Failed to connect:", err)
                    vim.notify("[MapStruct] Failed to connect: " .. err, vim.log.levels.ERROR)
                    state.connected = false
                    close_socket()
                    finish(false, err)
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
                        M.disconnect("Socket read error: " .. read_err)
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
                        M.disconnect("Server disconnected")
                    end)
                end
            end)

            -- Start heartbeat timer
            start_heartbeat()

            vim.schedule(function()
                finish(true, nil)
            end)
        end)
    end

    local function wait_for_socket()
        if connect_token ~= state.connect_token then
            return
        end

        if socket_exists(socket_path) then
            open_pipe()
            return
        end

        if uv.now() - started_at >= config.connect_timeout_ms then
            finish(false, "Socket file not found: " .. socket_path)
            return
        end

        vim.defer_fn(wait_for_socket, config.connect_poll_interval_ms)
    end

    wait_for_socket()

    return true
end

--- Send a JSON IPC request to the server.
---@param method string
---@param params? table
---@param callback? fun(result?: table, err?: string)
function M.request(method, params, callback)
    if not state.connected or not state.socket_fd then
        if callback then
            callback(nil, "Not connected")
        end
        return
    end

    local request_id = generate_request_id()
    local request = {
        id = request_id,
        method = method,
        params = params or {},
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
                log.warn("IPC write failed:", err)
                M.disconnect(err)
            end)
        end
    end)
end

--- Disconnect from the server and fail pending requests.
---@param reason? string
function M.disconnect(reason)
    stop_heartbeat()
    state.connect_token = state.connect_token + 1

    close_socket()

    state.connected = false
    fail_pending_requests(reason or "Disconnected")
    state.read_buffer = ""
end

--- Check if connected.
---@return boolean
function M.is_connected()
    return state.connected
end

--- Get connection status.
---@return table
function M.get_status()
    return {
        connected = state.connected,
        socket_path = config.socket_path,
        pending_requests = vim.tbl_count(state.pending_requests),
    }
end

return M
