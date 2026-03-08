# MapStruct Path Go-to-Definition

Navigate to field/method definitions from MapStruct `@Mapping` path expressions.

## Features

- **Navigate nested paths**: Jump from `person.address.city` directly to the `city` field in the `Address` class
- **Smart field detection**: Finds fields, getters, setters, and builder methods
- **Inner class support**: Handles inner classes correctly
- **Auto-completion powered**: Uses the same path resolution as MapStruct completions

## Usage

### Command

Place cursor on any path item in a `@Mapping` annotation:

```java
@Mapping(source = "person.address.city", target = "cityName")
                              ^--- cursor here
```

Run: `:MapStructGotoDefinition`

Result: Opens `Address.java` and jumps to the `city` field/getter.

### Via Lua API

```lua
local mapstruct = require("utils.java.mapstruct")
mapstruct.goto_path_definition()
```

### Keymap Example

```lua
vim.keymap.set("n", "gd", function()
    if require("utils.java.mapstruct").is_in_mapping_context({}) then
        require("utils.java.mapstruct").goto_path_definition()
    else
        vim.lsp.buf.definition()  -- Fallback to LSP
    end
end, { desc = "Go to definition (MapStruct-aware)" })
```

## How It Works

1. **Extract path**: Parses `person.address.city` into `path="person.address"` and `member="city"`
2. **Get context**: Determines if in `@Mapping` and gets source/target info
3. **Navigate path**: Uses MapStruct server to navigate `person → address` and get the `Address` class FQN
4. **Load class**: Uses jdtls to open the `Address` class file
5. **Find field**: Searches for the `city` field, getter, or setter
6. **Jump**: Moves cursor to the definition and centers screen

## Supported Patterns

The function can find:

- Direct fields: `private String city;`
- Getters: `public String getCity()`
- Record accessors: `public String city()`
- Setters: `public void setCity(String city)`
- Builder methods: `public Builder city(String city)`
- Fluent setters: `public Address city(String city) { ... }`

## Requirements

- Active jdtls LSP connection
- MapStruct server running (auto-starts if needed)
- Cursor in `@Mapping` or `@ValueMapping` annotation

## Error Messages

- **"No path member found under cursor"**: Cursor not on a valid path item
- **"Not in a valid @Mapping annotation"**: Outside MapStruct annotation
- **"Failed to get completions"**: Server error (check MapStruct server status)
- **"Field not found in class"**: Field exists in path but not found in opened class file

## Implementation Details

See `lua/utils/java/mapstruct/path_item_goto.lua` for the full implementation.

Main function: `goto_path_item_definitions()`

Helper functions:
- `get_mapping_path_under_cursor()` - Extracts path from cursor position
- `find_field_position()` - Searches for field/method in class file

## Examples

### Simple Path

```java
@Mapping(source = "name", target = "fullName")
              ^--- cursor
```
→ Jumps to `name` field in source class

### Nested Path

```java
@Mapping(source = "person.address.street", target = "streetName")
                           ^--- cursor
```
→ Jumps to `address` field/getter in `Person` class

```java
@Mapping(source = "person.address.street", target = "streetName")
                                   ^--- cursor
```
→ Jumps to `street` field in `Address` class

### Target Path

```java
@Mapping(source = "cityName", target = "address.city")
                                              ^--- cursor
```
→ Jumps to `city` field/setter in target class
