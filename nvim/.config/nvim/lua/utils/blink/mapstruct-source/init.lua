-- MapStruct Completion Source for blink.cmp
-- Provides path completion for MapStruct @Mapping annotations

local server = require("utils.blink.mapstruct-source.server")
local ipc_client = require("utils.blink.mapstruct-source.ipc_client")
local context = require("utils.blink.mapstruct-source.context")
local log = require("utils.logging-util").new({ name = "MapStruct", filename = "mapstruct-source.log" })

--- @class blink.cmp.Source
local source = {}

-- Initialize the source
function source.new(opts)
    local self = setmetatable({}, { __index = source })
    self.opts = opts or {}
    
    -- Debug: log received options
    log.info("MapStruct source initialized with opts:", vim.inspect(self.opts))

    -- Validate options
    if not self.opts.jar_path then
        vim.notify("[MapStruct] jar_path is required in blink.cmp config", vim.log.levels.ERROR)
        return self
    end

    -- Ensure jar exists
    local jar_path = vim.fn.expand(self.opts.jar_path)
    if vim.fn.filereadable(jar_path) ~= 1 then
        vim.notify("[MapStruct] jar_path not found: " .. jar_path, vim.log.levels.ERROR)
        return self
    end

    self.jar_path = jar_path
    self.use_jdtls_classpath = self.opts.use_jdtls_classpath ~= false -- default true
    self.java_cmd = self.opts.java_cmd or "java"
    self.server_started = false

    -- Setup auto-cleanup on VimLeavePre
    vim.api.nvim_create_autocmd("VimLeavePre", {
        callback = function()
            server.stop()
        end,
    })

    return self
end

-- Enable the source only for Java files with *Mapper.java naming pattern
function source:enabled()
    local filetype = vim.bo.filetype
    if filetype ~= "java" then
        return false
    end

    -- Check if filename ends with Mapper.java
    local filename = vim.fn.expand("%:t")
    return filename:match("Mapper%.java$") ~= nil
end

-- Trigger on dot character and letters (for typing inside annotation strings)
function source:get_trigger_characters()
    local chars = { "." }

    -- Add a-z and A-Z to trigger completion when typing inside annotation strings
    -- for i = string.byte("a"), string.byte("z") do
    --     table.insert(chars, string.char(i))
    -- end
    -- for i = string.byte("A"), string.byte("Z") do
    --     table.insert(chars, string.char(i))
    -- end

    return chars
end

-- Process completion results into blink.cmp items
function source:process_completions(result, callback, completion_ctx)
    if not result or not result.completions then
        callback({ items = {}, is_incomplete_forward = false, is_incomplete_backward = false })
        return
    end

    -- Helper function to simplify type names
    local function simplify_type(type_name)
        if not type_name then
            return "Unknown"
        end
        -- Extract simple name from fully qualified name
        -- e.g., "java.lang.String" -> "String"
        local simple = type_name:match("%.([^%.]+)$") or type_name
        return simple
    end

    -- Helper function to get full type with package
    local function format_type_with_package(type_name)
        if not type_name then
            return "Unknown"
        end
        -- If it's a java.lang type, just show simple name
        if type_name:match("^java%.lang%.") then
            return simplify_type(type_name)
        end
        return type_name
    end

    -- Convert to blink.cmp items
    local items = {}
    local completions = result.completions or {}

    for _, field_info in ipairs(completions) do
        -- Use appropriate kind based on field kind
        local kind
        local kind_label

        if field_info.kind == "PARAMETER" then
            -- Method parameter - show as Variable
            kind = require("blink.cmp.types").CompletionItemKind.Variable
            kind_label = "Parameter"
        elseif field_info.kind == "GETTER" then
            -- Getter method - show as Field (MapStruct uses property notation)
            kind = require("blink.cmp.types").CompletionItemKind.Field
            kind_label = "Getter Method"
        elseif field_info.kind == "SETTER" then
            -- Setter method - show as Property (target mappings)
            kind = require("blink.cmp.types").CompletionItemKind.Property
            kind_label = "Setter Method"
        else
            -- FIELD or unknown - show as Field
            kind = require("blink.cmp.types").CompletionItemKind.Field
            kind_label = "Field"
        end

        local simple_type = simplify_type(field_info.type)
        local full_type = format_type_with_package(field_info.type)

        local item = {
            label = field_info.name,
            -- Show type in label_description column
            labelDetails = {
                description = simple_type,
            },
            kind = kind,
            insertTextFormat = vim.lsp.protocol.InsertTextFormat.PlainText,
            insertText = field_info.name,
            -- Detailed documentation
            documentation = {
                kind = "markdown",
                value = string.format(
                    "**%s**: %s\n**Type:** `%s`\n**Kind:** %s%s%s\n**Path:** `%s%s`",
                    kind_label,
                    field_info.name,
                    full_type,
                    field_info.kind,
                    result.simpleName and ("\n**Source Class:** `" .. result.simpleName .. "`") or "",
                    result.packageName and ("\n**Package:** `" .. result.packageName .. "`") or "",
                    completion_ctx and completion_ctx.path_expression or "",
                    field_info.name
                ),
            },
            -- Store additional data for potential future use
            data = {
                mapstruct_field_type = field_info.type,
                mapstruct_field_kind = field_info.kind,
                mapstruct_class_name = result.className,
                mapstruct_package = result.packageName,
            },
        }
        table.insert(items, item)
    end

    callback({
        items = items,
        is_incomplete_forward = false,
        is_incomplete_backward = false,
    })
end

-- Ensure server is running
function source:ensure_server_running(callback)
    if server.is_running() and ipc_client.is_connected() then
        callback(true)
        return
    end

    -- If server_started is true but server is not actually running, it means it crashed
    -- Reset the flag so we can restart
    if self.server_started and not server.is_running() then
        log.warn("Server was marked as started but is not running - resetting flag")
        self.server_started = false
    end

    if self.server_started then
        -- Server is starting, wait a bit
        vim.defer_fn(function()
            local is_running = server.is_running() and ipc_client.is_connected()
            if not is_running then
                log.warn("Server failed to start within timeout")
                self.server_started = false -- Reset flag if startup failed
            end
            callback(is_running)
        end, 500)
        return
    end

    -- If using jdtls classpath, wait for jdtls to be ready first
    if self.use_jdtls_classpath then
        local classpath_util = require("utils.blink.mapstruct-source.classpath-util")
        if not classpath_util.is_jdtls_ready() then
            log.warn("jdtls is not ready yet - cannot start server without complete classpath")

            -- Don't start server with incomplete classpath
            -- Just return false and user can try again after jdtls is ready
            callback(false)

            -- Show notification only once per session
            if not self._jdtls_warning_shown then
                self._jdtls_warning_shown = true
                vim.notify(
                    "[MapStruct] Waiting for jdtls to initialize. Please try completion again in a moment.",
                    vim.log.levels.WARN
                )
            end
            return
        end

        -- Reset warning flag when jdtls is ready
        self._jdtls_warning_shown = false
    end

    self:start_server_internal(callback)
end

-- Internal method to actually start the server (after jdtls check)
function source:start_server_internal(callback)
    self.server_started = true

    server.start(self.jar_path, {
        java_cmd = self.java_cmd,
        use_jdtls_classpath = self.use_jdtls_classpath,
        classpath = self.opts.classpath,
        log_level = self.opts.log_level,
    }, function(success, err)
        if not success then
            log.error("Failed to start server:", err)
            vim.notify("[MapStruct] Failed to start server: " .. (err or "unknown error"), vim.log.levels.ERROR)
            self.server_started = false -- Reset flag on failure
            callback(false)
        else
            log.info("Server started successfully")
            callback(true)
        end
    end)
end

-- Get completions
function source:get_completions(ctx, callback)
    -- Extract completion context using Treesitter
    -- ctx.line is 1-indexed, ctx.character is 0-indexed (LSP style)
    local completion_ctx = context.get_completion_context(
        ctx.bufnr,
        ctx.cursor[1] - 1, -- Convert to 0-indexed for Treesitter
        ctx.cursor[2]
    )

    if not completion_ctx then
        -- Not in a valid MapStruct context
        callback({ items = {}, is_incomplete_forward = false, is_incomplete_backward = false })
        return function() end
    end

    -- Ensure server is running
    self:ensure_server_running(function(running)
        if not running then
            log.error("Server is not running - cannot provide completions")
            callback({ items = {}, is_incomplete_forward = false, is_incomplete_backward = false })
            return
        end

        -- Build request based on attribute type
        local request_params
        if completion_ctx.attribute_type == "target" then
            -- Target: navigate directly into the target class fields
            -- Send single source with synthetic name - server will navigate directly into the type
            request_params = {
                sources = { { name = "$target", type = completion_ctx.class_name } },
                pathExpression = completion_ctx.path_expression,
                isEnum = completion_ctx.is_enum or false,
            }
        else
            -- Source: use new protocol with sources array
            request_params = {
                sources = completion_ctx.sources, -- Array of {name, type}
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
                    self.server_started = false
                    server.cleanup()

                    -- Clear classpath cache to get fresh classpath on restart
                    if self.use_jdtls_classpath then
                        local classpath_util = require("utils.blink.mapstruct-source.classpath-util")
                        classpath_util.clear_cache()
                        log.info("Cleared classpath cache for server restart")
                    end

                    self:ensure_server_running(function(running)
                        if running then
                            log.info("Server restarted, retrying request...")
                            ipc_client.request("explore_path", request_params, function(retry_result, retry_err)
                                if retry_err then
                                    log.error("Retry failed:", retry_err)
                                    callback({
                                        items = {},
                                        is_incomplete_forward = false,
                                        is_incomplete_backward = false,
                                    })
                                    return
                                end

                                if not retry_result or not retry_result.completions then
                                    callback({
                                        items = {},
                                        is_incomplete_forward = false,
                                        is_incomplete_backward = false,
                                    })
                                    return
                                end

                                -- Process successful retry
                                self:process_completions(retry_result, callback, completion_ctx)
                            end)
                        else
                            log.error("Failed to restart server")
                            callback({ items = {}, is_incomplete_forward = false, is_incomplete_backward = false })
                        end
                    end)
                    return
                end

                callback({ items = {}, is_incomplete_forward = false, is_incomplete_backward = false })
                return
            end

            -- Process completions and return items
            self:process_completions(result, callback, completion_ctx)
        end)
    end)

    -- Return cancel function
    return function()
        -- TODO: Implement request cancellation if needed
    end
end

-- Resolve additional details for an item
function source:resolve(item, callback)
    -- For now, we provide all information upfront
    -- In the future, we could fetch detailed type information here
    callback(item)
end

-- Setup user commands for debugging and control
vim.api.nvim_create_user_command("MapStructStatus", function()
    local status = server.get_status()
    local classpath_util = require("utils.blink.mapstruct-source.classpath-util")
    local jdtls_ready = classpath_util.is_jdtls_ready()

    print("MapStruct Server Status:")
    print("  Server Running: " .. tostring(status.running))
    print("  Server Starting: " .. tostring(status.starting))
    print("  Socket: " .. (status.socket_path or "N/A"))
    print("  Jar: " .. (status.jar_path or "N/A"))
    print("  IPC Connected: " .. tostring(status.ipc_status.connected))
    print("  Pending Requests: " .. (status.ipc_status.pending_requests or 0))
    print("")
    print("jdtls Status:")
    print("  Ready: " .. tostring(jdtls_ready))
    if not jdtls_ready then
        print("  Note: Server will not start until jdtls is ready")
    end
end, { desc = "Show MapStruct server status" })

vim.api.nvim_create_user_command("MapStructRestart", function()
    server.restart(function(success)
        if success then
            log.info("Server restarted successfully")
            vim.notify("[MapStruct] Server restarted successfully", vim.log.levels.INFO)
        else
            vim.notify("[MapStruct] Failed to restart server", vim.log.levels.ERROR)
        end
    end)
end, { desc = "Restart MapStruct server" })

vim.api.nvim_create_user_command("MapStructStop", function()
    server.stop(function()
        log.info("Server stopped")
        vim.notify("[MapStruct] Server stopped", vim.log.levels.INFO)
    end)
end, { desc = "Stop MapStruct server" })

vim.api.nvim_create_user_command("MapStructPing", function()
    if not ipc_client.is_connected() then
        log.warn("Not connected to server")
        vim.notify("[MapStruct] Not connected to server", vim.log.levels.WARN)
        return
    end

    ipc_client.request("ping", {}, function(result, err)
        if err then
            vim.notify("[MapStruct] Ping failed: " .. err, vim.log.levels.ERROR)
        else
            log.info("Pong:", result)
            vim.notify("[MapStruct] Pong: " .. vim.inspect(result), vim.log.levels.INFO)
        end
    end)
end, { desc = "Ping MapStruct server" })

return source
