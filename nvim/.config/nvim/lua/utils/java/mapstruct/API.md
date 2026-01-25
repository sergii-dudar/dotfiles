# MapStruct Module - API Reference

Quick reference for the isolated MapStruct module API.

## Module Loading

```lua
local mapstruct = require("utils.java.mapstruct")
```

## Initialization

### Default Options

The module comes with sensible defaults and will auto-initialize on first use if `setup()` is not called manually.

```lua
DEFAULT_OPTS = {
    jar_path = "~/tools/java-extensions/mapstruct/mapstruct-path-explorer.jar",
    use_jdtls_classpath = true,
    classpath = nil,
    java_cmd = "java",
    log_level = vim.log.levels.WARN,
    log_file = "~/.local/state/nvim/mapstruct-source-server.log",
}
```

### `setup(opts)`

Initialize the module (optional - auto-initializes with defaults on first use).

```lua
mapstruct.setup({
    jar_path = "~/path/to/mapstruct-path-explorer.jar",  -- Optional if default exists
    use_jdtls_classpath = true,                          -- Default: true
    java_cmd = "java",                                   -- Default: "java"
    classpath = "/custom/path",                          -- Optional: fallback classpath
    log_level = "INFO",                                  -- Default: vim.log.levels.WARN
    log_file = "~/.local/share/nvim/mapstruct.log",     -- Optional: Java log file
})
```

**Options are merged with defaults** - you only need to specify what you want to override.

**Returns:** `boolean` - success status

### Auto-Initialization

If you don't call `setup()`, the module will **automatically initialize with default options** on the first call to `get_completions()`. This means you can use the module without any configuration if the default jar path exists.

```lua
local mapstruct = require("utils.java.mapstruct")

-- No setup needed! Auto-initializes on first use
mapstruct.get_completions({}, function(result, err)
    -- Handle completions
end)
```

### User Commands

User commands (`:MapStructStatus`, `:MapStructRestart`, `:MapStructStop`, `:MapStructPing`) are **automatically created** when `setup()` is called (either manually or via auto-initialization).

## Core Functions

### `get_completions(params, callback)`

Get MapStruct field/method completions at cursor position.

```lua
mapstruct.get_completions({
    bufnr = 0,                           -- Optional: buffer number
    row = 10,                            -- Optional: 0-indexed row
    col = 20,                            -- Optional: 0-indexed column
}, function(result, error)
    if error then
        print("Error:", error)
        return
    end
    
    -- result.completions = [
    --   { name = "firstName", type = "java.lang.String", kind = "GETTER" },
    --   { name = "age", type = "int", kind = "FIELD" },
    --   ...
    -- ]
    -- result.className = "com.example.Person"
    -- result.simpleName = "Person"
    -- result.packageName = "com.example"
    
    for _, field in ipairs(result.completions) do
        print(field.name, field.type, field.kind)
    end
end)
```

**Field kinds:** `FIELD`, `GETTER`, `SETTER`, `PARAMETER`

### `get_context(params)`

Get completion context at cursor position (without querying server).

```lua
local ctx = mapstruct.get_context({
    bufnr = 0,
    row = 10,
    col = 20,
})

if ctx then
    -- ctx.sources = [{ name = "user", type = "com.example.User" }]
    -- ctx.class_name = "com.example.UserDTO"  (for target)
    -- ctx.path_expression = "address."
    -- ctx.attribute_type = "source" | "target"
    -- ctx.is_enum = false
    print("In @Mapping annotation:", ctx.attribute_type)
end
```

**Returns:** Context object or `nil`

## Server Control

### `get_status()`

Get module and server status.

```lua
local status = mapstruct.get_status()

-- status.initialized = true
-- status.server_running = true
-- status.server_starting = false
-- status.socket_path = "/tmp/mapstruct-ipc-12345.sock"
-- status.jar_path = "/path/to/jar"
-- status.ipc_connected = true
-- status.pending_requests = 0
-- status.jdtls_ready = true

print("Server running:", status.server_running)
print("jdtls ready:", status.jdtls_ready)
```

### `restart(callback)`

Restart the Java IPC server.

```lua
mapstruct.restart(function(success)
    if success then
        print("Server restarted")
    else
        print("Restart failed")
    end
end)
```

### `stop(callback)`

Stop the Java IPC server.

```lua
mapstruct.stop(function()
    print("Server stopped")
end)
```

### `ping(callback)`

Ping the server to check connectivity.

```lua
mapstruct.ping(function(result, error)
    if error then
        print("Ping failed:", error)
    else
        print("Pong received:", vim.inspect(result))
    end
end)
```

## Utility Functions

### `is_in_mapping_context(params)`

Check if cursor is in a valid `@Mapping` or `@ValueMapping` annotation.

```lua
local is_valid = mapstruct.is_in_mapping_context({
    bufnr = 0,
    row = 10,
    col = 20,
})

if is_valid then
    print("Cursor is in @Mapping annotation")
end
```

**Returns:** `boolean`

### `is_mapper_file(bufnr)`

Check if buffer is a Java mapper file (ends with `Mapper.java` or `Builder.java`).

```lua
if mapstruct.is_mapper_file() then
    print("This is a mapper file")
end

-- Or check specific buffer
if mapstruct.is_mapper_file(5) then
    print("Buffer 5 is a mapper file")
end
```

**Returns:** `boolean`

### `is_initialized()`

Check if module has been initialized.

```lua
if not mapstruct.is_initialized() then
    mapstruct.setup({ jar_path = "..." })
end
```

**Returns:** `boolean`

### `set_log_level(level)`

Set log level for all MapStruct modules.

```lua
mapstruct.set_log_level("DEBUG")
-- or
mapstruct.set_log_level(vim.log.levels.DEBUG)
```

**Levels:** `"DEBUG"`, `"INFO"`, `"WARN"`, `"ERROR"` or numeric

## Complete Example

```lua
local mapstruct = require("utils.java.mapstruct")

-- Initialize
if not mapstruct.setup({
    jar_path = vim.fn.expand("~/mapstruct-path-explorer.jar"),
    log_level = "INFO",
}) then
    print("Failed to initialize MapStruct")
    return
end

-- Create a mapping to get completions
vim.keymap.set("n", "<leader>mc", function()
    -- Check if in valid context
    if not mapstruct.is_in_mapping_context({}) then
        vim.notify("Not in @Mapping annotation")
        return
    end
    
    -- Get completions
    mapstruct.get_completions({}, function(result, err)
        if err then
            vim.notify("Error: " .. err, vim.log.levels.ERROR)
            return
        end
        
        -- Show completions in quickfix
        local items = {}
        for _, field in ipairs(result.completions or {}) do
            table.insert(items, {
                text = string.format("%s: %s (%s)", 
                    field.name, 
                    field.type, 
                    field.kind
                ),
            })
        end
        
        vim.fn.setqflist(items)
        vim.cmd("copen")
    end)
end, { desc = "MapStruct: Get completions" })

-- Create status command
vim.api.nvim_create_user_command("MSStatus", function()
    local status = mapstruct.get_status()
    print(vim.inspect(status))
end, {})

-- Auto-restart on jdtls attach
vim.api.nvim_create_autocmd("LspAttach", {
    callback = function(args)
        local client = vim.lsp.get_client_by_id(args.data.client_id)
        if client and client.name == "jdtls" then
            -- Wait a bit for jdtls to fully initialize
            vim.defer_fn(function()
                mapstruct.restart(function(success)
                    if success then
                        vim.notify("MapStruct server restarted with jdtls classpath")
                    end
                end)
            end, 2000)
        end
    end,
})
```

## Error Handling

All async functions use error-first callbacks:

```lua
callback(result, error)
```

Common errors:
- `"MapStruct module not initialized"` - Call `setup()` first
- `"Not in a valid MapStruct @Mapping context"` - Cursor not in annotation
- `"Server is not running"` - Server failed to start or crashed
- `"Not connected"` - IPC connection lost
- `"timeout"` - Request took too long

## See Also

- [README.md](./README.md) - Full documentation with architecture details
- [MIGRATION.md](./MIGRATION.md) - Refactoring summary
- `utils.blink.mapstruct-source` - blink.cmp integration example
