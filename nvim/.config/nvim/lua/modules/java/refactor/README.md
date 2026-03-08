# Java Refactoring for Neovim

Intelligent Java refactoring tool that automatically updates imports, package declarations, and type references when moving or renaming Java files and packages in Neovim.

> **Note:** This plugin is currently integrated into a personal dotfiles configuration but is planned to be extracted into a standalone Neovim plugin in a separate repository.

## Overview

This tool provides automatic refactoring capabilities for Java projects in Neovim, eliminating the manual work of updating imports, package declarations, and references when restructuring your codebase. It's designed to work seamlessly with file managers like [fyler.nvim](https://github.com/dmtrKovalenko/fyler.nvim) for batch operations and can handle individual file moves.

## Features

### ✨ Core Capabilities

- **Automatic Package Declaration Updates** - Updates `package` statements in moved files
- **Import Management** - Adds/updates/removes imports across the entire project
- **Type Reference Updates** - Updates all references to moved types (class names, interfaces, etc.)
- **Full Qualified Name Updates** - Handles fully qualified type references in code and config files
- **Sibling File Awareness** - Smart handling when moving multiple files together from the same package
- **Directory/Package Moves** - Supports moving entire packages with correct project-wide replacements
- **Cross-platform** - Works on both macOS and Linux with GNU sed

### 🎯 Key Features

- **Individual file processing** - File moves don't trigger unwanted package-level changes
- **Explicit directory move support** - Moving directories triggers appropriate package-level refactoring
- **Buffer management** - Automatically handles open buffers, reloading changed files
- **Smart import cleanup** - Removes unnecessary same-package imports
- **Batch processing** - Efficient handling of multiple file operations at once
- **Comprehensive logging** - Detailed logs for debugging and verification

## Requirements

### Dependencies

- **Neovim** >= 0.11.5 (for Lua APIs)
- **ripgrep** (`rg`) - Fast text searching (Rust-based alternative to `grep`)
- **fd** - Fast file finder (Rust-based alternative to `find`)
- **GNU sed** - Text stream editing
    - macOS: Install via `brew install gnu-sed` (provides `gsed`)
    - Linux: Already available as `sed`

### File Manager Integration

While not required, this tool is designed to integrate with file managers:

- **[fyler.nvim](https://github.com/dmtrKovalenko/fyler.nvim)** - Recommended for batch operations
- Can also work with manual file operations via `process_single_file_change()`

## Installation

### Prerequisites

```bash
# macOS
brew install ripgrep fd gnu-sed

# Linux (Debian/Ubuntu)
apt-get install ripgrep fd-find

# Linux (Arch)
pacman -S ripgrep fd
```

**Note for Ubuntu/Debian:** The `fd` binary is named `fdfind` to avoid conflicts. Create a symlink:

```bash
ln -s $(which fdfind) ~/.local/bin/fd
```

### Neovim Setup

Since this is currently part of a dotfiles configuration, the structure is:

```
nvim/.config/nvim/lua/modules/java/refactor/
├── init.lua      # Main orchestration module
├── import-fixer.lua             # Import management
├── sibling-usage-fixer.lua      # Sibling file reference handling
└── README.md                    # This file
```

#### Configuration Example

**1. Setup autocmd for file moves:**

```lua
-- lua/config/autocmds.lua
local java_refactor = require("modules.java.refactor")

-- Example: Integrate with fyler.nvim
vim.api.nvim_create_autocmd("User", {
    pattern = "FylerBatchOperationComplete",
    callback = function()
        java_refactor.process_registerd_changes()
    end,
})
```

**2. Integrate with your file manager:**

```lua
-- lua/plugins/navigation/fyler-nvim.lua
return {
    "dmtrKovalenko/fyler.nvim",
    config = function()
        local java_refactor = require("modules.java.refactor")
        local fyler = require("fyler")

        fyler.setup({
            on_register_file_move = function(from, to)
                java_refactor.register_change(from, to)
            end,
        })
    end,
}
```

## How It Works

### Architecture

The refactoring system consists of three main modules:

#### 1. **init.lua** (Orchestration)

- Registers file/directory move operations
- Determines operation type (file move vs directory move)
- Builds and executes refactoring commands
- Manages buffer reloading and cleanup

#### 2. **import-fixer.lua** (Import Management)

- Adds imports to files in the old package that reference moved types
- Handles multiple sibling files being moved together
- Ensures import statements are correctly placed after package declarations

#### 3. **sibling-usage-fixer.lua** (Sibling Reference Handling)

- Updates references when multiple files are moved together
- Adds necessary imports for sibling file usage
- Replaces type names with correct references

### Operation Flow

```
File Move Detected
       ↓
Register Change
       ↓
Batch Processing Triggered
       ↓
Determine Operation Type
   ├─→ File Move: Individual processing
   └─→ Directory Move: Package-level processing
       ↓
Build Refactoring Operations
   ├─→ Shell commands (sed, rg)
   └─→ Lua functions (import/sibling fixes)
       ↓
Execute Operations
       ↓
Reload Changed Buffers
       ↓
Complete
```

### File Move Processing

When processing **individual file moves**, the tool:

1. **Updates the moved file:**
    - Changes type declaration (class/interface/enum name)
    - Updates package declaration
    - Fixes package declaration in the file

2. **Updates files that import the moved type:**
    - Finds files with `import old.package.TypeName`
    - Updates type references (where imported)
    - Updates import statements to new package

3. **Updates fully qualified references:**
    - Finds all `old.package.TypeName` references
    - Updates to `new.package.TypeName`
    - Handles code, config files (YAML, properties), etc.

4. **Handles sibling files:**
    - If multiple files moved from same directory
    - Updates imports for sibling type references
    - Replaces sibling type names correctly

5. **Manages old package imports:**
    - Adds imports to files in old package that use the moved type
    - Removes same-package imports (no longer needed)

### Directory Move Processing

When moving **entire directories/packages**, the tool:

1. **Performs project-wide replacement:**
    - Uses `rg` to find all references to the old package
    - Uses `sed` to replace package names across all files
    - Handles package declarations, imports, and fully qualified names

2. **Updates file paths:**
    - Replaces resource paths and file system paths
    - Ensures consistency across configuration files

### Smart Detection

The tool intelligently determines operation type:

- **Explicit directory moves** → Package-level refactoring
- **File moves** (any count) → Individual file processing
- **No false package moves** → Won't treat partial directory moves as package moves

## Usage

### API Methods

#### `register_change(src, dst)`

Registers a file or directory move operation.

```lua
local java_refactor = require("modules.java.refactor")

-- Register a single file move
java_refactor.register_change(
    "/path/to/src/main/java/com/example/service/UserService.java",
    "/path/to/src/main/java/com/example/service/impl/UserServiceImpl.java"
)

-- Register multiple moves (batch)
java_refactor.register_change(from1, to1)
java_refactor.register_change(from2, to2)
java_refactor.register_change(from3, to3)
```

#### `process_registerd_changes()`

Processes all registered changes in a batch.

```lua
-- After registering all changes, trigger batch processing
java_refactor.process_registerd_changes()
```

#### `process_single_file_change(src, dst)`

Process a single file move immediately (non-batch mode).

```lua
java_refactor.process_single_file_change(
    "/path/to/src/main/java/com/example/User.java",
    "/path/to/src/main/java/com/example/model/User.java"
)
```

### Usage Examples

#### Example 1: Moving a Single File

```lua
-- Move UserService.java from service/ to service/impl/
java_refactor.register_change(
    "src/main/java/com/example/service/UserService.java",
    "src/main/java/com/example/service/impl/UserService.java"
)
java_refactor.process_registerd_changes()
```

**What happens:**

- ✅ Package declaration updated in `UserService.java`
- ✅ Import `com.example.service.UserService` → `com.example.service.impl.UserService` in other files
- ✅ Type references updated in files that import it
- ✅ Files in `service/` package that use `UserService` get updated imports
- ❌ **NOT affected:** Other files in `service/` package keep their original package declarations

#### Example 2: Moving Multiple Files

```lua
-- Move 2 files from service/ to service/impl/
java_refactor.register_change(
    "src/main/java/com/example/service/UserService.java",
    "src/main/java/com/example/service/impl/UserService.java"
)
java_refactor.register_change(
    "src/main/java/com/example/service/ProductService.java",
    "src/main/java/com/example/service/impl/ProductService.java"
)
java_refactor.process_registerd_changes()
```

**What happens:**

- ✅ Each file processed individually
- ✅ Both files recognized as siblings (moved from same directory)
- ✅ If `UserService` uses `ProductService`, imports handled correctly
- ❌ **NOT affected:** Other files remaining in `service/` package

#### Example 3: Moving an Entire Package (Directory)

```lua
-- Move service/ directory to usecase/service/
java_refactor.register_change(
    "src/main/java/com/example/service/",
    "src/main/java/com/example/usecase/service/"
)
java_refactor.process_registerd_changes()
```

**What happens:**

- ✅ Project-wide replacement: `com.example.service` → `com.example.usecase.service`
- ✅ All files in the moved directory updated
- ✅ All imports across the entire project updated
- ✅ Config files (YAML, properties) with package references updated

#### Example 4: Renaming a File

```lua
-- Rename User.java to UserEntity.java
java_refactor.register_change(
    "src/main/java/com/example/model/User.java",
    "src/main/java/com/example/model/UserEntity.java"
)
java_refactor.process_registerd_changes()
```

**What happens:**

- ✅ Class declaration: `class User` → `class UserEntity`
- ✅ Import updates: `com.example.model.User` → `com.example.model.UserEntity`
- ✅ Type references: `User user = ...` → `UserEntity user = ...` (in files that import it)
- ✅ Fully qualified names updated

## Configuration

### Package Roots

The tool automatically detects standard Java package roots:

- `src/main/java/`
- `src/test/java/`

To customize, modify the `package_roots` table in `init.lua`:

```lua
local package_roots = {
    "src/main/java/",
    "src/test/java/",
    "src/it/java/",  -- Integration tests
    -- Add more as needed
}
```

### Logging

Logs are written to `java-refactor.log` in the Neovim cache directory.

To adjust log level, modify the logger in each module:

```lua
local log = logging.new({
    name = "java-refactor",
    filename = "java-refactor.log",
    level = "DEBUG"  -- DEBUG, INFO, WARN, ERROR
})
```

### Test Mode

For automated testing, enable test mode to skip UI:

```lua
local java_refactor = require("modules.java.refactor")
java_refactor.test_mode = true
```

## Technical Details

### Regex Patterns

The tool uses carefully crafted regex patterns to avoid false positives:

**Package boundary pattern:**

```lua
-- Matches:
--   com.example.service; (package declaration)
--   com.example.service.ClassName (import with class - dot + uppercase)
--   com.example.service" (string literal)
-- Does NOT match:
--   com.example.service.impl (subpackage - dot + lowercase)

pattern = 's/%s([;$"[:space:]]|\\.[A-Z]|$)/%s\\1/g'
```

### Cross-Platform Sed

The tool uses GNU sed on both platforms:

```lua
-- macOS: gsed (GNU sed via Homebrew)
-- Linux: sed (already GNU sed)
local sed = vim.loop.os_uname().sysname == "Darwin" and "gsed" or "sed"
```

This ensures consistent behavior across operating systems.

### Buffer Management

Open buffers are handled intelligently:

- Before refactoring: All affected file paths are tracked
- After refactoring: Changed buffers are reloaded
- Deleted files: Buffers are closed and deleted

## Limitations & Known Issues

### Current Limitations

1. **Single module projects** - Multi-module Maven/Gradle projects may need adjustment
2. **Manual verification recommended** - Always review changes, especially for large refactorings
3. **No undo support** - Changes are made directly to files (use git!)
4. **Standard package structure** - Expects standard `src/main/java/` layout

### Best Practices

- ✅ **Use version control** - Always commit before refactoring
- ✅ **Review changes** - Check git diff after operations
- ✅ **Test compilation** - Run `mvn compile` or `gradle build` after refactoring
- ✅ **Batch operations** - Process related moves together for better sibling handling

## Troubleshooting

### Issue: Imports not updating

**Solution:** Check that:

1. Files are under a recognized package root (`src/main/java/`)
2. Ripgrep is installed and in PATH
3. Check logs: `~/.cache/nvim/java-refactor.log`

### Issue: Package declarations in wrong files changed

**Solution:** This was fixed in the latest version. Ensure you're using the version without package move inference for file moves.

### Issue: sed command not found (macOS)

**Solution:** Install GNU sed:

```bash
brew install gnu-sed
```

### Issue: Some references not updated

**Solution:** The tool handles:

- Standard imports
- Fully qualified class names
- Type references in code

It may not handle:

- Reflection strings (`Class.forName("com.example.OldClass")`)
- Comments with class names
- Non-standard references

## Future Plans

### Planned Features

- [ ] Extract to standalone Neovim plugin
- [ ] Support for multi-module projects
- [ ] Undo/rollback support
- [ ] Dry-run mode (preview changes)
- [ ] Integration with more file managers (neo-tree, oil.nvim, etc.)
- [ ] Refactoring within single file (extract method, etc.)

### Standalone Plugin

This functionality is planned to be extracted into a standalone plugin:

- Separate git repository
- Proper plugin structure
- Installation via plugin managers (lazy.nvim, packer, etc.)
- Better documentation and examples
- Test suite

## Contributing

Currently, this is part of a personal dotfiles configuration. Once extracted to a standalone plugin, contributions will be welcome!

## Acknowledgments

- Inspired by IDE refactoring tools (IntelliJ IDEA, Eclipse)
- Built for the Neovim community
- Uses [fyler.nvim](https://github.com/dmtrKovalenko/fyler.nvim) for batch operations

---

**Status:** 🚧 Active Development - Used daily in production Java development