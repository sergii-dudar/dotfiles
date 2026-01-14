# MapStruct Completion Source for blink.cmp

A custom completion source for [blink.cmp](https://github.com/saghen/blink.cmp) that provides intelligent path completion for MapStruct `@Mapping` annotations.

## Features

- **Intelligent Path Completion**: Auto-completes field paths in `@Mapping(source = "...")` and `@Mapping(target = "...")`
- **Rich Type Information**: Shows field types, kind (field/getter), class name, and package in completion menu
- **Visual Distinction**: Different icons for fields vs getter methods
- **Treesitter-based Context Detection**: Uses Neovim's Treesitter to accurately parse Java code and detect MapStruct annotations
- **Automatic jdtls Integration**: Automatically uses classpath from `nvim-jdtls`, including all project modules and dependencies
- **IPC Communication**: Fast Unix domain socket communication with Java backend
- **Lifecycle Management**: Automatically starts/stops server with Neovim
- **Multi-module Support**: Works with Maven/Gradle multi-module projects

## Completion Menu Display

The completion menu shows comprehensive information about each field:

```
 street      String      Field      [MS]
 city        String      Method     [MS]
󰜢 zipCode    Integer     Field      [MS]
```

- **Icon**: Different icons for fields (󰜢) vs methods ()
- **Label**: Field/method name
- **Type**: Simplified type name (e.g., "String" instead of "java.lang.String")
- **Kind**: "Field" or "Method" indicating the member type
- **Source**: `[MS]` badge indicating MapStruct completion source

Hover over any completion item to see detailed documentation including:

- Full type with package
- Source class and package
- Complete path expression
- Whether it's a field or getter method

## Architecture

```
┌──────────────────────────────────────────────┐
│ init.lua                                      │
│ - blink.cmp source interface                 │
│ - Coordinates all modules                    │
└───────┬──────────────────────────────────────┘
        │
        ├─► context.lua
        │   - Treesitter-based Java AST parsing
        │   - Extracts class name and path expression
        │   - Resolves imports and FQN
        │
        ├─► server.lua
        │   - Starts/stops Java IPC server
        │   - Queries jdtls for classpath
        │   - Manages server lifecycle
        │
        └─► ipc_client.lua
            - Unix domain socket communication
            - JSON protocol (vim.json)
            - Heartbeat monitoring
```

## Installation

### 1. Build the Java Server

```bash
cd /path/to/mapstruct-path-explorer
mvn clean package
# Creates: target/mapstruct-path-explorer.jar
```

### 2. Configure blink.cmp

The source is already configured in `lua/plugins/editor/blink-cmp.lua`. Update the jar path:

```lua
mapstruct = {
    name = "mapstruct",
    module = "utils.blink.mapstruct-source",
    opts = {
        -- Required: path to mapstruct-path-explorer.jar
        jar_path = "~/path/to/mapstruct-path-explorer/target/mapstruct-path-explorer.jar",

        -- Optional: use jdtls classpath (default: true)
        use_jdtls_classpath = true,
    },
},
```

### 3. Verify Setup

1. Open a Java file with MapStruct mappers
2. Start typing in a `@Mapping` annotation:
    ```java
    @Mapping(source = "user.address.|")
    ```
3. Completions should appear after typing the dot

## Usage

### Basic Example

```java
@Mapper
public interface UserMapper {
    // Type "user." and see field suggestions
    @Mapping(source = "user.address.street", target = "streetName")
    @Mapping(source = "user.address.city", target = "cityName")
    UserDTO toDto(User user);
}
```

### User Commands

The source provides several commands for debugging and control:

- `:MapStructStatus` - Show server status and connection info
- `:MapStructPing` - Test connection to server
- `:MapStructRestart` - Restart server with updated classpath
- `:MapStructStop` - Stop server

### Keymaps (Optional)

Add to your Java ftplugin or config:

```lua
vim.keymap.set('n', '<leader>ms', ':MapStructStatus<CR>', { desc = 'MapStruct: Status' })
vim.keymap.set('n', '<leader>mr', ':MapStructRestart<CR>', { desc = 'MapStruct: Restart' })
vim.keymap.set('n', '<leader>mp', ':MapStructPing<CR>', { desc = 'MapStruct: Ping' })
```

## How It Works

1. **Trigger**: When you type a dot (`.`) in a Java file
2. **Context Detection**: Treesitter parses the Java AST to check if you're in a `@Mapping` annotation
3. **Class Resolution**: Extracts the source/target class from the method signature and resolves its fully qualified name
4. **Path Extraction**: Determines what path you've typed so far (e.g., `user.address.`)
5. **Server Request**: Sends request to Java server with class name and path
6. **Response**: Server uses reflection to explore available fields and returns suggestions
7. **Display**: Converts to blink.cmp format and displays in completion menu

## Configuration Options

```lua
opts = {
    -- Required: Path to the jar file
    jar_path = "~/path/to/mapstruct-path-explorer.jar",

    -- Use jdtls classpath (includes all project modules and dependencies)
    -- Default: true
    use_jdtls_classpath = true,

    -- Manual classpath (used as fallback if jdtls fails)
    -- Default: nil
    classpath = "/path/to/additional/classes",

    -- Custom Java command
    -- Default: "java"
    java_cmd = "/usr/bin/java",
}
```

## Troubleshooting

### No completions appearing

1. Check server status: `:MapStructStatus`
2. Verify jar path is correct in config
3. Ensure you're in a `@Mapping` annotation
4. Check that jdtls is running: `:LspInfo`

### Server fails to start

1. Check Java is installed: `java -version` (requires Java 17+)
2. Verify jar file exists and is readable
3. Check Neovim messages: `:messages`
4. Try manual start to see errors:
    ```bash
    java -cp /path/to/mapstruct-path-explorer.jar com.dsm.mapstruct.IpcServer /tmp/test.sock
    ```

### Classpath issues

1. Verify jdtls is running and project is loaded
2. Try restarting: `:MapStructRestart`
3. Check jdtls workspace: `~/.cache/jdtls/`
4. As fallback, provide manual classpath in opts

### Connection issues

1. Check socket file exists: `ls -la /tmp/mapstruct-ipc-*.sock`
2. Verify no firewall blocking Unix sockets
3. Restart server: `:MapStructRestart`

## Development

### Module Structure

- **init.lua**: Main blink.cmp source implementation
- **context.lua**: Treesitter-based context extraction
- **server.lua**: Java server lifecycle management
- **ipc_client.lua**: Unix socket IPC client

### Testing Context Detection

```lua
-- In a Java file with MapStruct annotations
local context = require('utils.blink.mapstruct-source.context')
local ctx = context.get_completion_context(0, vim.fn.line('.') - 1, vim.fn.col('.'))
print(vim.inspect(ctx))
-- Should print: { class_name = "...", path_expression = "...", ... }
```

### Debugging IPC

Enable debug logging in `server.lua` by changing `vim.log.levels.DEBUG` to `vim.log.levels.INFO`.

## Requirements

- Neovim 0.9+
- Java 17+ (for Unix domain socket support)
- `mfussenegger/nvim-jdtls` (for automatic classpath resolution)
- `saghen/blink.cmp`
- Maven or Gradle project with MapStruct

## Future Enhancements

- [ ] Cache completions for better performance
- [ ] Support for `@Mappings` and other MapStruct annotations
- [ ] Support for expression completion