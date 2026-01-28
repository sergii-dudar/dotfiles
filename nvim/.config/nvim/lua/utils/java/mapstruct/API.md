# MapStruct Module - API Reference

Quick reference for the isolated MapStruct module API.

## Module Loading

```lua
local mapstruct = require("utils.java.mapstruct")
```

## Request Flow - Start to Finish

How a completion request works (optimized for speed):

```
User types in @Mapping annotation
         ↓
┌────────────────────────────────────────────────────────────┐
│ 1. Context Analysis (context.lua) - ~1-5ms                │
│    • Uses Treesitter to parse Java AST                    │
│    • Finds @Mapping/@ValueMapping annotation              │
│    • Extracts source/target types and path expression     │
│    • Detects @MappingTarget from source code              │
│    • Returns nil if not valid → early exit                │
└────────────────────────────────────────────────────────────┘
         ↓
┌────────────────────────────────────────────────────────────┐
│ 2. Type Resolution (context.lua) - with caching           │
│    • Type Source Cache: typeName → JAR path(5min|renew)   │
│    • Method Params Cache: signature → types (5min|renew)  │
│    • Uses javap only on cache miss                        │
│    • Auto-cleanup every 60s                               │
└────────────────────────────────────────────────────────────┘
         ↓
┌────────────────────────────────────────────────────────────┐
│ 3. Server Startup (server.lua) - only if not running      │
│    • Gets jdtls classpath from all project modules        │
│    • Filters SLF4J providers (logback, log4j, etc)        │
│    • Starts Java IPC server on Unix socket                │
└────────────────────────────────────────────────────────────┘
         ↓
┌────────────────────────────────────────────────────────────┐
│ 4. IPC Request (ipc_client.lua) - Unix socket             │
│    • Sends JSON request to Java server                    │
│    • Fast local communication (no network)                │
└────────────────────────────────────────────────────────────┘
         ↓
┌────────────────────────────────────────────────────────────┐
│ 5. Java Processing (IpcServer.java)                       │
│    • Loads classes using reflection                       │
│    • Extracts fields/getters/setters                      │
│    • Returns completion items                             │
└────────────────────────────────────────────────────────────┘
         ↓
┌────────────────────────────────────────────────────────────┐
│ 6. Result                                                  │
│    • Completions: [{name, type, kind}, ...]               │
│    • Enriched with context for consumers                  │
└────────────────────────────────────────────────────────────┘
```

### Key Components

**context.lua** - Fast context analysis

- Treesitter AST parsing (~1-5ms)
- Type source cache (5min) - avoids IPC calls
- Method parameters cache (5min) - avoids javap calls
- javap integration for fully qualified types

**server.lua** - Server lifecycle

- JDTLS classpath integration
- SLF4J conflict filtering
- Unix socket IPC server

**ipc_client.lua** - Communication

- JSON-based protocol
- Async callbacks
- Request timeout handling

**jdtls-classpath-util.lua** - Classpath management

- Gets classpath from all project modules
- Multi-module support

### User Commands

```
:MapStructStart   - Manually start server
:MapStructStatus  - Show server/cache status
:MapStructRestart - Restart server
:MapStructStop    - Stop server
:MapStructPing    - Check connectivity
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

- `utils.blink.mapstruct-source` - blink.cmp integration
