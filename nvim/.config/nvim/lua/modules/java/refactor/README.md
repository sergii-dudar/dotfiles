# Java Refactoring for Neovim

Automatic Java refactoring after file/package moves — updates imports, package declarations,
type references, and test↔src mirror sync across your project.

> Part of a personal dotfiles config. Planned to become a standalone plugin.

## Features

- **Package declaration updates** — fixes `package` statements in moved files
- **Import management** — adds/updates/removes imports project-wide
- **Type reference updates** — renames class/interface/enum usages where imported
- **FQN updates** — handles fully qualified names in Java, YAML, properties files
- **Test↔Src sync** — automatically mirrors package structure between main and test
- **Root package rename** — supports deep multi-segment transformations (e.g., `ua/payments` → `ua/gov/test/other`)
- **Buffer management** — switches current buffer to new location, reopens others
- **Empty dir cleanup** — removes orphaned empty package directories
- **Batch processing** — efficient handling of multiple files/directories at once
- **Cross-platform** — macOS (gsed) and Linux (sed)

## Requirements

```bash
# macOS
brew install ripgrep fd gnu-sed

# Linux (Debian/Ubuntu)
apt-get install ripgrep fd-find

# Linux (Arch)
pacman -S ripgrep fd
```

## Module Structure

```
lua/modules/java/refactor/
├── init.lua              — Entry point & orchestrator (public API)
├── constants.lua         — Package roots, shell utilities, sed patterns
├── canonical.lua         — Canonical transformation detection & correction
├── mirror-sync.lua       — Test↔src mirror: compute, dedup, move, cleanup
├── buffer-manager.lua    — Track, delete, reopen buffers around moves
├── cmd-builder.lua       — Build shell/Lua operations for file & package fixes
├── executor.lua          — Terminal execution, test-mode support
├── import-fixer.lua      — Import management for moved files
└── sibling-usage-fixer.lua — Cross-reference fixes for batch moves
```

## Integration

### fyler.nvim (batch file manager)

```lua
-- lua/plugins/navigation/fyler-nvim.lua
return {
    "dmtrKovalenko/fyler.nvim",
    config = function()
        local java_refactor = require("modules.java.refactor")
        require("fyler").setup({
            on_rename = function(src, dst)
                java_refactor.register_change(src, dst)
            end,
        })
    end,
}
```

### Autocmd trigger

```lua
-- lua/config/autocmds.lua
vim.api.nvim_create_autocmd("User", {
    pattern = "FylerBufUnload",
    callback = function()
        if require("utils.java.java-common").is_java_project() then
            require("modules.java.refactor").process_registerd_changes()
        end
    end,
})
```

## API

### `register_change(src, dst)`

Register a file or directory move. Call once per moved item.

```lua
local refactor = require("modules.java.refactor")
refactor.register_change(
    "/project/src/main/java/com/example/service/UserService.java",
    "/project/src/main/java/com/example/service/impl/UserServiceImpl.java"
)
```

### `process_registerd_changes()`

Process all registered changes as a batch. Triggers the full pipeline:
module detection → buffer tracking → canonical correction → mirror sync →
command building → execution → buffer reopen → cleanup.

```lua
refactor.process_registerd_changes()
```

### `process_single_file_change(src, dst)`

Convenience: register + process in one call.

```lua
refactor.process_single_file_change(src, dst)
```

### `test_mode`

Set `true` for headless execution (no terminal UI, no spinner). Designed for automated testing.

```lua
refactor.test_mode = true
local success = refactor.process_registerd_changes() -- returns boolean
```

## How It Works

### File Move (individual)

1. Fix type declaration (`class Foo` → `class Bar`)
2. Fix constructor names (for move+rename)
3. Update type symbols in files that import the old package
4. Fix sibling references (when multiple files moved together)
5. Update fully qualified names across all file types
6. Fix package declaration in the moved file
7. Remove same-package imports (now unnecessary)
8. Add imports to files in the old package that reference the moved type
9. Update file/resource paths

### Package/Directory Move

1. Replace package FQNs project-wide (`old.pkg` → `new.pkg` including subpackages)
2. Update path references in config files

### Root Package Rename (with test↔src sync)

When renaming a root package in either `src/main/java/` or `src/test/java/`:

1. **Canonical detection** — extracts the true old→new prefix from file-level moves
2. **Destination correction** — fixes intermediate directory events with wrong destinations
3. **Parent-of-canonical filter** — skips partial/intermediate filesystem events
4. **Mirror sync** — physically moves the counterpart tree (test↔main)
5. **Mirror dedup** — keeps shallowest correct mirror per branch
6. **Package fixes** — sed updates package declarations/imports in all moved files
7. **Empty dir cleanup** — removes orphaned directories from old structure

## Logging

Logs are written to `~/.local/state/nvim/java-refactor.log`. Contains detailed information
about every step of the refactoring pipeline. Useful for debugging unexpected behavior.

## Limitations

- **Single module per batch** — all changes must be in the same Maven/Gradle module
- **Standard layout assumed** — expects `src/main/java/`, `src/test/java/`
- **No undo** — changes are made directly to files (use git!)
- **No preview/dry-run** — planned for future
- **Reflection strings** — `Class.forName("com.example.Foo")` not handled

## Best Practices

- Always commit before refactoring
- Review `git diff` after operations
- Run `mvn compile` / `gradle build` to verify
- Process related moves together for better sibling handling
- Check logs if results are unexpected
