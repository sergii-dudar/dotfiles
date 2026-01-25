# MapStruct Module - Isolated Java MapStruct Utility

This directory contains the isolated MapStruct completion module that can be used independently of any completion framework.

## Module Structure

```
utils/java/mapstruct/
├── init.lua         - Main module API (isolated from blink.cmp)
├── context.lua      - Treesitter-based context extraction
├── server.lua       - Java IPC server lifecycle management
└── ipc_client.lua   - Unix domain socket IPC client
```

## Features

- **Framework-independent**: Core logic is isolated from blink.cmp
- **Automatic server management**: Starts/stops Java server automatically
- **jdtls integration**: Uses jdtls for classpath resolution
- **Context detection**: Treesitter-based parsing of MapStruct annotations
- **Multi-parameter support**: Handles mappers with multiple source parameters
- **@MappingTarget support**: Correctly handles void return type mappers

## Usage

### Standalone Usage

```lua
local mapstruct = require("utils.java.mapstruct")

-- Initialize the module
mapstruct.setup({
    jar_path = "~/path/to/mapstruct-path-explorer.jar",
    use_jdtls_classpath = true,  -- default: true
    java_cmd = "java",           -- default: "java"
    log_level = "INFO",          -- optional
})

-- Optionally setup user commands
mapstruct.setup_commands()  -- Creates :MapStructStatus, :MapStructRestart, etc.

-- Check if cursor is in a valid MapStruct context
local is_valid = mapstruct.is_in_mapping_context({
    bufnr = 0,
    row = vim.api.nvim_win_get_cursor(0)[1] - 1,
    col = vim.api.nvim_win_get_cursor(0)[2],
})

-- Get completions
mapstruct.get_completions({
    bufnr = 0,
    row = vim.api.nvim_win_get_cursor(0)[1] - 1,
    col = vim.api.nvim_win_get_cursor(0)[2],
}, function(result, err)
    if err then
        print("Error:", err)
        return
    end

    -- result.completions: array of { name, type, kind }
    -- result.className: fully qualified class name
    -- result.simpleName: simple class name
    -- result.packageName: package name
    for _, field in ipairs(result.completions or {}) do
        print(field.name, field.type, field.kind)
    end
end)

-- Get server status
local status = mapstruct.get_status()
print("Server running:", status.server_running)
print("jdtls ready:", status.jdtls_ready)

-- Restart server
mapstruct.restart(function(success)
    print("Restart:", success)
end)

-- Stop server
mapstruct.stop()
```

### With blink.cmp

The `utils.blink.mapstruct-source` module is a thin wrapper that adapts the isolated module for blink.cmp:

```lua
-- In blink.cmp config
sources = {
    providers = {
        mapstruct = {
            name = "mapstruct",
            module = "utils.blink.mapstruct-source",
            opts = {
                jar_path = "~/path/to/mapstruct-path-explorer.jar",
                use_jdtls_classpath = true,
                log_level = "INFO",
            },
        },
    },
}
```

## API Reference

### `setup(opts)`

Initialize the MapStruct module.

**Parameters:**

- `opts.jar_path` (string, required): Path to mapstruct-path-explorer.jar
- `opts.use_jdtls_classpath` (boolean, default: true): Use jdtls for classpath
- `opts.java_cmd` (string, default: "java"): Java command
- `opts.classpath` (string, optional): Manual classpath (fallback)
- `opts.log_level` (string, optional): Log level ("DEBUG", "INFO", "WARN", "ERROR")
- `opts.log_file` (string, optional): Java server log file path

**Returns:** boolean (success)

### `get_completions(params, callback)`

Get MapStruct completions at cursor position.

**Parameters:**

- `params.bufnr` (number, default: current buffer): Buffer number
- `params.row` (number, default: cursor row): 0-indexed row
- `params.col` (number, default: cursor col): 0-indexed column
- `callback(result, error)`: Callback function

**Callback parameters:**

- `result.completions`: Array of `{ name, type, kind }`
    - `kind`: "FIELD", "GETTER", "SETTER", or "PARAMETER"
- `result.className`: Fully qualified class name
- `result.simpleName`: Simple class name
- `result.packageName`: Package name
- `error`: Error message or nil

### `get_context(params)`

Get completion context at cursor position.

**Parameters:**

- `params.bufnr`, `params.row`, `params.col` (same as `get_completions`)

**Returns:** Context object or nil

- `sources`: Array of `{ name, type }` for source parameters
- `class_name`: Target class name (for target attribute)
- `path_expression`: Current path being typed
- `attribute_type`: "source" or "target"
- `is_enum`: boolean (for @ValueMapping)

### `get_status()`

Get module and server status.

**Returns:** Status object

- `initialized`: boolean
- `server_running`: boolean
- `server_starting`: boolean
- `socket_path`: string or nil
- `jar_path`: string or nil
- `ipc_connected`: boolean
- `pending_requests`: number
- `jdtls_ready`: boolean

### `is_in_mapping_context(params)`

Check if cursor is in a valid MapStruct @Mapping annotation.

**Parameters:**

- `params` (same as `get_completions`)

**Returns:** boolean

### `is_mapper_file(bufnr)`

Check if file is a Java mapper file (ends with Mapper.java or Builder.java).

**Parameters:**

- `bufnr` (number, default: current buffer)

**Returns:** boolean

### `restart(callback)`

Restart the Java server.

**Parameters:**

- `callback(success)`: Optional callback

### `stop(callback)`

Stop the Java server.

**Parameters:**

- `callback()`: Optional callback

### `ping(callback)`

Ping the server to check connectivity.

**Parameters:**

- `callback(result, error)`: Callback function

### `set_log_level(level)`

Set log level for all MapStruct modules.

**Parameters:**

- `level` (string or number): "DEBUG", "INFO", "WARN", "ERROR" or numeric level

## Dependencies

- `utils.java.jdtls-classpath-util` - jdtls classpath resolution
- `utils.logging-util` - Logging utilities
- `nvim-jdtls` - Java LSP (optional, for automatic classpath)
- Java 25+ - For Unix domain socket support

## Architecture

The module is designed with clear separation of concerns:

1. **init.lua**: Public API, coordinates all components
2. **context.lua**: Treesitter parsing, javap integration for type resolution
3. **server.lua**: Java process management, classpath building
4. **ipc_client.lua**: Low-level IPC communication, connection management

This separation allows:

- Using the core functionality without blink.cmp
- Testing components independently
- Reusing IPC infrastructure for other tools
- Easy integration with other completion frameworks

## Examples

See `utils/blink/mapstruct-source/init.lua` for a complete integration example with blink.cmp.

For standalone usage examples, see the test files in the project.