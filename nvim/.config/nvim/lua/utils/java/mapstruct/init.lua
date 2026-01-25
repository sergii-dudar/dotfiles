-- Provides MapStruct completion functionality independent of any completion framework
-- Can be used with blink.cmp, nvim-cmp, or standalone

local server = require("utils.java.mapstruct.server")
local ipc_client = require("utils.java.mapstruct.ipc_client")
local context = require("utils.java.mapstruct.context")
local classpath_util = require("utils.java.jdtls-classpath-util")
local logging_util = require("utils.logging-util")
local log = logging_util.new({ name = "MapStruct", filename = "mapstruct-source.log" })

local M = {}

-- Module state
local state = {
    initialized = false,
    jar_path = nil,
    use_jdtls_classpath = true,
    java_cmd = "java",
    server_started = false,
    opts = {},
}

-- Set log level for all mapstruct modules
function M.set_log_level(level)
    local numeric_level = logging_util.level_to_number(level)
    log.set_level(numeric_level)
    server.set_log_level(numeric_level)
    ipc_client.set_log_level(numeric_level)
    context.set_log_level(numeric_level)
    classpath_util.set_log_level(numeric_level)
end

-- Initialize the MapStruct module
-- opts: { jar_path, use_jdtls_classpath, java_cmd, classpath, log_level, log_file }
function M.setup(opts)
    opts = opts or {}

    -- Validate required options
    if not opts.jar_path then
        vim.notify("[MapStruct] jar_path is required in setup", vim.log.levels.ERROR)
        return false
    end

    -- Ensure jar exists
    local jar_path = vim.fn.expand(opts.jar_path)
    if vim.fn.filereadable(jar_path) ~= 1 then
        vim.notify("[MapStruct] jar_path not found: " .. jar_path, vim.log.levels.ERROR)
        return false
    end

    -- Store configuration
    state.jar_path = jar_path
    state.use_jdtls_classpath = opts.use_jdtls_classpath ~= false -- default true
    state.java_cmd = opts.java_cmd or "java"
    state.opts = opts
    state.initialized = true

    -- Set log level if specified
    if opts.log_level then
        M.set_log_level(opts.log_level)
        log.info("MapStruct module initialized with log level:", opts.log_level)
    end

    log.info("MapStruct module initialized with opts:", vim.inspect(opts))

    -- Setup auto-cleanup on VimLeavePre
    vim.api.nvim_create_autocmd("VimLeavePre", {
        callback = function()
            server.stop()
        end,
    })

    return true
end

-- Check if module is initialized
function M.is_initialized()
    return state.initialized
end

-- Ensure server is running
local function ensure_server_running(callback)
    if server.is_running() and ipc_client.is_connected() then
        callback(true)
        return
    end

    -- If server_started is true but server is not actually running, it means it crashed
    if state.server_started and not server.is_running() then
        log.warn("Server was marked as started but is not running - resetting flag")
        state.server_started = false
    end

    if state.server_started then
        -- Server is starting, wait a bit
        vim.defer_fn(function()
            local is_running = server.is_running() and ipc_client.is_connected()
            if not is_running then
                log.warn("Server failed to start within timeout")
                state.server_started = false
            end
            callback(is_running)
        end, 500)
        return
    end

    -- If using jdtls classpath, wait for jdtls to be ready first
    if state.use_jdtls_classpath then
        if not classpath_util.is_jdtls_ready() then
            log.warn("jdtls is not ready yet - cannot start server without complete classpath")
            callback(false)
            return
        end
    end

    -- Start server
    state.server_started = true

    server.start(state.jar_path, {
        java_cmd = state.java_cmd,
        use_jdtls_classpath = state.use_jdtls_classpath,
        classpath = state.opts.classpath,
        log_level = state.opts.log_level,
        log_file = state.opts.log_file,
    }, function(success, err)
        if not success then
            log.error("Failed to start server:", err)
            vim.notify("[MapStruct] Failed to start server: " .. (err or "unknown error"), vim.log.levels.ERROR)
            state.server_started = false
            callback(false)
        else
            log.info("Server started successfully")
            callback(true)
        end
    end)
end

-- Get completions for MapStruct @Mapping annotations
-- params: { bufnr, row, col }
-- callback: function(completions, error)
-- completions format: { completions = {...}, className, simpleName, packageName }
function M.get_completions(params, callback)
    if not state.initialized then
        callback(nil, "MapStruct module not initialized. Call setup() first.")
        return
    end

    params = params or {}
    local bufnr = params.bufnr or vim.api.nvim_get_current_buf()
    local row = params.row or (vim.api.nvim_win_get_cursor(0)[1] - 1) -- 0-indexed
    local col = params.col or vim.api.nvim_win_get_cursor(0)[2] -- 0-indexed

    -- Extract completion context using Treesitter
    local completion_ctx = context.get_completion_context(bufnr, row, col)

    if not completion_ctx then
        -- Not in a valid MapStruct context
        callback(nil, "Not in a valid MapStruct @Mapping context")
        return
    end

    -- Ensure server is running
    ensure_server_running(function(running)
        if not running then
            log.error("Server is not running - cannot provide completions")
            callback(nil, "Server is not running")
            return
        end

        -- Build request based on attribute type
        local request_params
        if completion_ctx.attribute_type == "target" then
            -- Target: navigate directly into the target class fields
            request_params = {
                sources = { { name = "$target", type = completion_ctx.class_name } },
                pathExpression = completion_ctx.path_expression,
                isEnum = completion_ctx.is_enum or false,
            }
        else
            -- Source: use new protocol with sources array
            request_params = {
                sources = completion_ctx.sources,
                pathExpression = completion_ctx.path_expression,
                isEnum = completion_ctx.is_enum or false,
            }
        end

        -- Request path exploration from server
        ipc_client.request("explore_path", request_params, function(result, err)
            if err then
                log.warn("Request failed:", err)

                -- If request failed, try to reconnect and retry once
                if err == "Not connected" or err == "timeout" then
                    log.info("Connection lost, attempting to restart server...")

                    -- Reset state
                    state.server_started = false
                    server.cleanup()

                    -- Clear classpath cache to get fresh classpath on restart
                    if state.use_jdtls_classpath then
                        classpath_util.clear_cache()
                        log.info("Cleared classpath cache for server restart")
                    end

                    ensure_server_running(function(retry_running)
                        if retry_running then
                            log.info("Server restarted, retrying request...")
                            ipc_client.request("explore_path", request_params, function(retry_result, retry_err)
                                if retry_err then
                                    log.error("Retry failed:", retry_err)
                                    callback(nil, retry_err)
                                    return
                                end

                                callback(retry_result, nil)
                            end)
                        else
                            log.error("Failed to restart server")
                            callback(nil, "Failed to restart server")
                        end
                    end)
                    return
                end

                callback(nil, err)
                return
            end

            -- Return completions
            callback(result, nil)
        end)
    end)
end

-- Get context information at cursor position
-- params: { bufnr, row, col }
-- Returns: completion context or nil
function M.get_context(params)
    params = params or {}
    local bufnr = params.bufnr or vim.api.nvim_get_current_buf()
    local row = params.row or (vim.api.nvim_win_get_cursor(0)[1] - 1)
    local col = params.col or vim.api.nvim_win_get_cursor(0)[2]

    return context.get_completion_context(bufnr, row, col)
end

-- Get server status
function M.get_status()
    local status = server.get_status()
    local jdtls_ready = classpath_util.is_jdtls_ready()

    return {
        initialized = state.initialized,
        server_running = status.running,
        server_starting = status.starting,
        socket_path = status.socket_path,
        jar_path = status.jar_path,
        ipc_connected = status.ipc_status.connected,
        pending_requests = status.ipc_status.pending_requests or 0,
        jdtls_ready = jdtls_ready,
    }
end

-- Restart the server
function M.restart(callback)
    server.restart(function(success)
        if callback then
            callback(success)
        end
    end)
end

-- Stop the server
function M.stop(callback)
    server.stop(function()
        if callback then
            callback()
        end
    end)
end

-- Ping the server
function M.ping(callback)
    if not ipc_client.is_connected() then
        if callback then
            callback(nil, "Not connected")
        end
        return
    end

    ipc_client.request("ping", {}, function(result, err)
        if callback then
            callback(result, err)
        end
    end)
end

-- Check if cursor is in a valid MapStruct context
-- params: { bufnr, row, col }
function M.is_in_mapping_context(params)
    local ctx = M.get_context(params)
    return ctx ~= nil
end

-- Check if file is a mapper file
function M.is_mapper_file(bufnr)
    bufnr = bufnr or vim.api.nvim_get_current_buf()
    local filetype = vim.bo[bufnr].filetype
    if filetype ~= "java" then
        return false
    end

    local filename = vim.fn.expand("%:t")
    return filename:match("Mapper%.java$") ~= nil or filename:match("Builder%.java$") ~= nil
end

-- Setup user commands for debugging and control
-- This is optional - commands can be created manually if needed
function M.setup_commands()
    vim.api.nvim_create_user_command("MapStructStatus", function()
        local status = M.get_status()

        print("MapStruct Server Status:")
        print("  Initialized: " .. tostring(status.initialized))
        print("  Server Running: " .. tostring(status.server_running))
        print("  Server Starting: " .. tostring(status.server_starting))
        print("  Socket: " .. (status.socket_path or "N/A"))
        print("  Jar: " .. (status.jar_path or "N/A"))
        print("  IPC Connected: " .. tostring(status.ipc_connected))
        print("  Pending Requests: " .. status.pending_requests)
        print("")
        print("jdtls Status:")
        print("  Ready: " .. tostring(status.jdtls_ready))
        if not status.jdtls_ready then
            print("  Note: Server will not start until jdtls is ready")
        end
    end, { desc = "Show MapStruct server status" })

    vim.api.nvim_create_user_command("MapStructRestart", function()
        M.restart(function(success)
            if success then
                log.info("Server restarted successfully")
                vim.notify("[MapStruct] Server restarted successfully", vim.log.levels.INFO)
            else
                vim.notify("[MapStruct] Failed to restart server", vim.log.levels.ERROR)
            end
        end)
    end, { desc = "Restart MapStruct server" })

    vim.api.nvim_create_user_command("MapStructStop", function()
        M.stop(function()
            log.info("Server stopped")
            vim.notify("[MapStruct] Server stopped", vim.log.levels.INFO)
        end)
    end, { desc = "Stop MapStruct server" })

    vim.api.nvim_create_user_command("MapStructPing", function()
        M.ping(function(result, err)
            if err then
                vim.notify("[MapStruct] Ping failed: " .. err, vim.log.levels.ERROR)
            else
                log.info("Pong:", result)
                vim.notify("[MapStruct] Pong: " .. vim.inspect(result), vim.log.levels.INFO)
            end
        end)
    end, { desc = "Ping MapStruct server" })
end

return M
