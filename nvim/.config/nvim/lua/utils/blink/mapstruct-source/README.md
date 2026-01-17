# MapStruct Completion Source for blink.cmp

A custom completion source for [blink.cmp](https://github.com/saghen/blink.cmp) that provides intelligent path completion for MapStruct `@Mapping` annotations.

## Features

- **Intelligent Path Completion**: Auto-completes field paths in `@Mapping(source = "...")`, `@Mapping(target = "...")`, and `@ValueMapping`
- **Rich Type Information**: Shows field types, kind (FIELD/GETTER/SETTER/PARAMETER), class name, and package in completion menu
- **Smart Context Detection**: Automatically detects source vs target context and shows appropriate completions (getters for source, setters for target)
- **Multi-parameter Mapper Support**: Suggests parameter names for mappers with multiple source parameters
- **@MappingTarget Support**: Handles void return type mappers with `@MappingTarget` parameters
- **Setter Detection**: Recognizes JavaBean-style setters, builder patterns, and fluent setters
- **Visual Distinction**: Different icons for fields, getters, setters, and parameters
- **Treesitter-based Context Detection**: Uses Neovim's Treesitter to accurately parse Java code and detect MapStruct annotations
- **Automatic jdtls Integration**: Automatically uses classpath from `nvim-jdtls`, including all project modules and dependencies
- **IPC Communication**: Fast Unix domain socket communication with Java backend
- **Lifecycle Management**: Automatically starts/stops server with Neovim
- **Multi-module Support**: Works with Maven/Gradle multi-module projects

## Completion Menu Display

The completion menu shows comprehensive information about each field:

**Source Completions** (showing getters):
```
 firstName   String      Getter     [MS]
 address     Address     Getter     [MS]
󰜢 age        int         Field      [MS]
```

**Target Completions** (showing setters):
```
 setName     String      Setter     [MS]
 setAge      int         Setter     [MS]
 title       String      Setter     [MS]  (builder-style)
```

**Multi-parameter Mapper** (showing parameters):
```
 person      Person      Parameter  [MS]
 order       Order       Parameter  [MS]
```

- **Icon**: Different icons for fields (󰜢), getters (), setters (), and parameters ()
- **Label**: Field/method/parameter name
- **Type**: Simplified type name (e.g., "String" instead of "java.lang.String")
- **Kind**: "Field", "Getter", "Setter", or "Parameter" indicating the member type
- **Source**: `[MS]` badge indicating MapStruct completion source

Hover over any completion item to see detailed documentation including:

- Full type with package
- Source class and package
- Complete path expression
- Member kind (field, getter, setter, or parameter)

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
mvn clean package -U
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

### Basic Examples

**Source Mapping** - Shows getters and fields:
```java
@Mapper
public interface UserMapper {
    // Type "user." and see getter/field suggestions
    @Mapping(source = "user.address.street", target = "streetName")
    @Mapping(source = "user.address.city", target = "cityName")
    UserDTO toDto(User user);
}
```

**Target Mapping** - Shows setters and fields:
```java
@Mapper
public interface UserMapper {
    // Type in target and see setter suggestions (converted from getters)
    @Mapping(source = "street", target = "user.address.street")
    User fromDto(UserDTO dto);
}
```

**Multi-parameter Mapper** - Shows parameter names first:
```java
@Mapper
public interface PersonMapper {
    // Empty path shows: "person", "order", "customName"
    // Type "person." to navigate into Person fields
    @Mapping(source = "person.firstName", target = "name")
    @Mapping(source = "order.orderId", target = "orderReference")
    CompletePersonDTO map(Person person, Order order, String customName);
}
```

**@MappingTarget Support** - Handles void methods:
```java
@Mapper
public interface UserMapper {
    // Target completions use PersonDTO type from @MappingTarget parameter
    @Mapping(source = "firstName", target = "name")
    void updateDto(@MappingTarget PersonDTO dto, Person person);
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

1. **jdtls Readiness Check**: Before starting the server, checks if jdtls is fully initialized and can provide classpath
   - If jdtls is not ready, shows a warning and does NOT start the server
   - User can try completion again after jdtls initializes (typically 5-15 seconds after opening Neovim)
   - This prevents starting with incomplete classpath that would cause ClassNotFoundException
2. **Server Startup**: Starts Java IPC server with complete classpath from jdtls (all modules and dependencies)
3. **Trigger**: When you type a dot (`.`) or any letter in a Java file within a MapStruct annotation
4. **Context Detection**: Treesitter parses the Java AST to check if you're in a `@Mapping` or `@ValueMapping` annotation
5. **Attribute Type Detection**: Determines if you're in `source`, `target`, or enum value context
6. **Parameter Resolution**:
   - For source mappings: Extracts all method parameters (excluding `@MappingTarget`)
   - For target mappings: Uses method return type or `@MappingTarget` parameter type for void methods
   - For multi-parameter mappers: Collects all source parameter names and types
7. **Path Extraction**: Determines what path you've typed so far (e.g., `person.address.`)
8. **Server Request**: Sends IPC request with:
   - Source parameters with names and fully qualified types
   - Path expression
   - Whether it's an enum value mapping
9. **Response**: Server uses reflection to:
   - Navigate through the object graph following the path
   - Return appropriate members: getters for source, setters for target, parameters for empty multi-param paths
   - Apply automatic GETTER→SETTER conversion for target context
10. **Auto-reconnection**: If server stops due to inactivity, automatically restarts on next completion request
11. **Display**: Converts to blink.cmp format with appropriate icons and kind labels

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

1. **Check jdtls status first**: Run `:MapStructStatus` to see if jdtls is ready
   - If "jdtls Ready: false", wait a few seconds for jdtls to initialize
   - Try completion again after jdtls shows as ready
2. Check server status in the same `:MapStructStatus` output
3. Verify jar path is correct in config
4. Ensure you're in a `@Mapping` annotation
5. Check that jdtls is running: `:LspInfo`

**Important**: The server will NOT start until jdtls is ready. This prevents ClassNotFoundException errors from incomplete classpath. If you get the message "Waiting for jdtls to initialize", just wait 5-15 seconds and try completion again.

### Server fails to start

1. Check Java is installed: `java -version` (requires Java 25+)
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

### ClassNotFoundException or "Error exploring path"

This happens when the server starts with incomplete classpath:

1. **Most common cause**: Server started before jdtls was ready
   - Solution: Wait for jdtls to initialize, then restart: `:MapStructRestart`
   - The plugin now prevents this by refusing to start without jdtls

2. **Multi-module projects**: Test classes in different modules
   - Ensure jdtls has loaded all project modules
   - Check `:MapStructStatus` shows jdtls as ready
   - Restart server to get fresh classpath: `:MapStructRestart`

3. **Project not compiled**: Classes don't exist yet
   - Build your project: `mvn compile` or `./gradlew build`
   - Restart server: `:MapStructRestart`

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

- Neovim 0.11+
- Java 25+ (for Unix domain socket support and SequencedCollection support)
- `mfussenegger/nvim-jdtls` (for automatic classpath resolution)
- `saghen/blink.cmp`
- Maven or Gradle project with MapStruct
