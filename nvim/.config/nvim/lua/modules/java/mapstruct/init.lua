-- MapStruct Path Explorer - Isolated Module
-- Provides MapStruct completion functionality independent of any completion framework
-- Can be used with blink.cmp, nvim-cmp, or standalone

local config = require("modules.java.mapstruct.config")
local server = require("modules.java.mapstruct.server")
local ipc_client = require("modules.java.mapstruct.ipc_client")
local context = require("modules.java.mapstruct.context")
local classpath_util = require("utils.java.jdtls-classpath-util")
local logging_util = require("utils.logging-util")
local log = logging_util.new({ name = "MapStruct", filename = "mapstruct-source.log" })
local spinner = require("utils.ui.spinner")

local M = {}

-- Module state
local state = {
    initialized = false,
    jar_path = nil,
    use_jdtls_classpath = config.defaults.use_jdtls_classpath,
    server_started = false,
    opts = config.get_defaults(),
}

local commands_setup = false
local cleanup_augroup = nil
local ensure_initialized
local ensure_server_running

--- Set log level for all MapStruct modules.
---@param level string|number
function M.set_log_level(level)
    local numeric_level = logging_util.level_to_number(level)
    log.set_level(numeric_level)
    server.set_log_level(numeric_level)
    ipc_client.set_log_level(numeric_level)
    context.set_log_level(numeric_level)
    classpath_util.set_log_level(numeric_level)
end

--- Return effective options after applying MapStruct defaults.
---@param opts? table
---@return table
function M.default_opts(opts)
    return config.merge(opts)
end

--- Setup user commands for debugging and control.
local function setup_commands()
    if commands_setup then
        return
    end

    commands_setup = true

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
    end, { desc = "Show MapStruct server status", force = true })

    vim.api.nvim_create_user_command("MapStructRestart", function()
        M.restart(function(success)
            if success then
                log.info("Server restarted successfully")
                vim.notify("[MapStruct] Server restarted successfully", vim.log.levels.INFO)
            else
                vim.notify("[MapStruct] Failed to restart server", vim.log.levels.ERROR)
            end
        end)
    end, { desc = "Restart MapStruct server", force = true })

    vim.api.nvim_create_user_command("MapStructStop", function()
        M.stop(function()
            log.info("Server stopped")
            vim.notify("[MapStruct] Server stopped", vim.log.levels.INFO)
        end)
    end, { desc = "Stop MapStruct server", force = true })

    vim.api.nvim_create_user_command("MapStructPing", function()
        M.ping(function(result, err)
            if err then
                vim.notify("[MapStruct] Ping failed: " .. err, vim.log.levels.ERROR)
            else
                log.info("Pong:", result)
                vim.notify("[MapStruct] Pong: " .. vim.inspect(result), vim.log.levels.INFO)
            end
        end)
    end, { desc = "Ping MapStruct server", force = true })

    vim.api.nvim_create_user_command("MapStructServerStart", function()
        ensure_initialized()
        if not state.initialized then
            vim.notify("[MapStruct] Module failed to initialize", vim.log.levels.ERROR)
            return
        end

        ensure_server_running(function(success, err)
            if success then
                log.info("Server started successfully")
                vim.notify("[MapStruct] Server started successfully", vim.log.levels.INFO)
            else
                vim.notify("[MapStruct] Failed to start server: " .. (err or "unknown error"), vim.log.levels.ERROR)
            end
        end)
    end, { desc = "Start MapStruct server", force = true })
end

--- Setup cleanup hooks for the MapStruct server and timers.
local function setup_cleanup_autocmd()
    if cleanup_augroup then
        return
    end

    cleanup_augroup = vim.api.nvim_create_augroup("MapStructCleanup", { clear = true })
    vim.api.nvim_create_autocmd("VimLeavePre", {
        group = cleanup_augroup,
        callback = function()
            context.stop_cleanup_timer()
            server.stop()
        end,
    })
end

--- Initialize the MapStruct module.
---@param opts? table
---@return boolean
function M.setup(opts)
    opts = config.merge(opts)

    -- Validate jar_path
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
    state.opts = opts
    state.initialized = true
    ipc_client.configure(opts)

    -- Set log level if specified
    if opts.log_level then
        M.set_log_level(opts.log_level)
        log.info("MapStruct module initialized with log level:", opts.log_level)
    end

    log.info("MapStruct module initialized with opts:", vim.inspect(opts))

    -- Setup user commands
    setup_commands()

    -- Setup auto-cleanup on VimLeavePre
    setup_cleanup_autocmd()

    return true
end

--- Auto-initialize with defaults if not already initialized.
ensure_initialized = function()
    if not state.initialized then
        log.info("Auto-initializing MapStruct module with default options")
        M.setup()
    end
end

--- Check if module is initialized.
---@return boolean
function M.is_initialized()
    return state.initialized
end

--- Ensure server is running.
---@param callback fun(success: boolean)
ensure_server_running = function(callback)
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

    spinner.start("🚀 " .. "Starting MapStruct Server...")
    server.start(state.jar_path, state.opts, function(success, err)
        if not success then
            log.error("Failed to start server:", err)
            vim.notify("[MapStruct] Failed to start server: " .. (err or "unknown error"), vim.log.levels.ERROR)
            state.server_started = false
            callback(false, err)
        else
            log.info("Server started successfully")
            callback(true)
        end
        spinner.stop(success, success and "MapStruct started" or "MapStruct failed to start")
    end)
end

--- Run an IPC request after starting the server, retrying once after connection loss.
---@param method string
---@param request_params table
---@param callback fun(result?: table, err?: string)
local function make_ipc_request(method, request_params, callback)
    -- Ensure server is running
    ensure_server_running(function(running)
        if not running then
            log.error("Server is not running - cannot make request")
            callback(nil, "Server is not running")
            return
        end

        -- Request from server
        ipc_client.request(method, request_params, function(result, err)
            if err then
                log.warn("Request failed:", err)

                -- If request failed, try to reconnect and retry once
                local connection_lost = type(err) == "string"
                    and (
                        err == "Not connected"
                        or err == "timeout"
                        or err == "Server cleanup"
                        or err == "Server disconnected"
                        or err:match("^Socket read error")
                        or err:match("^Heartbeat failed")
                    )

                if connection_lost then
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
                            ipc_client.request(method, request_params, function(retry_result, retry_err)
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

            -- Return result
            callback(result, nil)
        end)
    end)
end

--- Get completions for MapStruct @Mapping annotations.
---@param params? { bufnr?: integer, row?: integer, col?: integer }
---@param callback fun(result?: table, err?: string)
function M.get_completions(params, callback)
    -- Auto-initialize if needed
    ensure_initialized()

    if not state.initialized then
        callback(nil, "MapStruct module failed to initialize")
        return
    end

    params = params or {}
    local bufnr = params.bufnr or vim.api.nvim_get_current_buf()
    local row = params.row or (vim.api.nvim_win_get_cursor(0)[1] - 1) -- 0-indexed
    local col = params.col or vim.api.nvim_win_get_cursor(0)[2] -- 0-indexed

    -- Extract completion context using Treesitter
    local completion_ctx_result = context.get_completion_context(bufnr, row, col)

    if not completion_ctx_result.ok then
        -- Not in a valid MapStruct context
        local msg = completion_ctx_result.message or "Not in a valid MapStruct @Mapping context"

        -- Only show notification for jdtls_not_ready, others are expected (cursor not in right place)
        if completion_ctx_result.reason == "jdtls_not_ready" then
            vim.notify("[MapStruct] " .. msg, vim.log.levels.WARN)
        end

        callback(nil, msg)
        return
    end

    local completion_ctx = completion_ctx_result.value

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

    -- Make request using base request logic with retry on compilation errors
    local max_retries = state.opts.path_retry_max_attempts
    local retry_delay_ms = state.opts.path_retry_initial_delay_ms
    local attempt = 0

    local function make_request_with_retry()
        attempt = attempt + 1

        make_ipc_request("explore_path", request_params, function(result, err)
            if result then
                -- Attach completion context to result for consumers
                result.completion_ctx = completion_ctx
                callback(result, err)
                return
            end

            -- Check if this is a compilation/classpath error that might resolve with a retry
            if err and type(err) == "string" and err:match("Error exploring path") then
                if attempt <= max_retries then
                    -- log.warn(
                    vim.notify(
                        string.format(
                            "Path exploration failed (attempt %d/%d), retrying in %dms: %s",
                            attempt,
                            max_retries,
                            retry_delay_ms,
                            err
                        )
                    )

                    -- Schedule retry with delay
                    vim.defer_fn(function()
                        make_request_with_retry()
                    end, retry_delay_ms)

                    -- Increase delay for next retry (exponential backoff)
                    retry_delay_ms = retry_delay_ms * 2
                else
                    -- Max retries exceeded, return the error
                    log.error(string.format("Path exploration failed after %d attempts: %s", max_retries, err))
                    vim.notify("[MapStruct] " .. err, vim.log.levels.ERROR)
                    callback(result, err)
                end
            else
                -- Other error, don't retry
                if err then
                    vim.notify("[MapStruct] " .. err, vim.log.levels.ERROR)
                end
                callback(result, err)
            end
        end)
    end

    make_request_with_retry()
end

--- Explore type source location.
---@param params? { typeName?: string }
---@param callback fun(result?: table, err?: string)
function M.explore_type_source(params, callback)
    ensure_initialized()

    if not state.initialized then
        callback(nil, "MapStruct module failed to initialize")
        return
    end

    params = params or {}
    local typeName = params.typeName

    if not typeName then
        callback(nil, "Missing required parameter: typeName")
        return
    end

    -- Build request
    local request_params = {
        typeName = typeName,
    }

    -- Make request using base request logic
    make_ipc_request("explore_type_source", request_params, callback)
end

--- Get context information at cursor position.
---@param params? { bufnr?: integer, row?: integer, col?: integer }
---@return table
function M.get_context(params)
    params = params or {}
    local bufnr = params.bufnr or vim.api.nvim_get_current_buf()
    local row = params.row or (vim.api.nvim_win_get_cursor(0)[1] - 1)
    local col = params.col or vim.api.nvim_win_get_cursor(0)[2]

    return context.get_completion_context(bufnr, row, col)
end

--- Get server status.
---@return table
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

--- Restart the server.
---@param callback? fun(success: boolean)
function M.restart(callback)
    server.restart(function(success)
        if callback then
            callback(success)
        end
    end)
end

--- Stop the server.
---@param callback? fun()
function M.stop(callback)
    server.stop(function()
        if callback then
            callback()
        end
    end)
end

--- Ping the server.
---@param callback? fun(result?: table, err?: string)
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

--- Check if cursor is in a valid MapStruct context.
---@param params? { bufnr?: integer, row?: integer, col?: integer }
---@return boolean
function M.is_in_mapping_context(params)
    local ctx = M.get_context(params)
    return ctx.ok == true
end

--- Check if file is a mapper or builder Java file.
---@param bufnr? integer
---@return boolean
function M.is_mapper_file(bufnr)
    bufnr = bufnr or vim.api.nvim_get_current_buf()
    local filetype = vim.bo[bufnr].filetype
    if filetype ~= "java" then
        return false
    end

    local filename = vim.fn.fnamemodify(vim.api.nvim_buf_get_name(bufnr), ":t")
    -- return filename:match("Mapper%.java$") ~= nil or filename:match("Builder%.java$") ~= nil
    return (filename:match("Mapper") or filename:match("Builder")) and filename:match("%.java$")
end

--- Check whether MapStruct path go-to-definition should handle the current cursor.
---@param params? { bufnr?: integer, row?: integer, col?: integer }
---@return boolean
function M.can_goto_path_definition(params)
    local path_goto = require("modules.java.mapstruct.path_item_goto")
    return path_goto.can_goto_path_item_definition(params)
end

--- Go to definition of a field/method in a MapStruct path.
---@param opts GoToMapStructOptions|nil of options
function M.goto_path_definition(opts)
    local path_goto = require("modules.java.mapstruct.path_item_goto")
    path_goto.goto_path_item_definitions(opts)
end

setup_commands()

return M
