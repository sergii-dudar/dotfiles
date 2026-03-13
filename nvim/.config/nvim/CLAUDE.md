# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

Personal Neovim configuration built on **LazyVim** (folke/LazyVim) distribution with **lazy.nvim** plugin manager. Heavily focused on Java/Spring Boot development with extensive custom tooling. Part of a larger dotfiles repo (stow-managed).

## Formatting

Lua files are formatted with **StyLua**: 4-space indentation, 120-column width, double quotes preferred. Config is in `stylua.toml`. Run `stylua .` to format.

## Architecture

### Entry Point

`init.lua` -> sets up `_G.dd` (Snacks debug), loads `utils.global-util` (sets `_G.global`), then bootstraps via `config.lazy`.

### Core Config (`lua/config/`)

- `lazy.lua` - Plugin spec imports: LazyVim base -> LazyVim extras -> custom plugin groups
- `options.lua` - Vim options + auto-session restore + Neo-tree open on startup
- `keymaps.lua` - Custom keymaps (leader=Space). Notable remaps: `K`/`J` = 8-line jump, `H`/`L` = `^`/`$`, `q` = disabled (no macro recording)
- `autocmds.lua` - Auto-save on focus loss, session save/restore (resession.nvim), Trouble quickfix integration, Overseer WatchRun command

### Plugin Organization (`lua/plugins/`)

Plugins are grouped by category, each imported as a separate spec in `lazy.lua`:
- `editor/` - LSP, completion (blink.cmp), formatting (conform.nvim), DAP, treesitter, git, linting
- `editor/java/` - JDTLS, Spring Boot LS, java-deps.nvim, Java file rename with package update
- `editor/lua/` - lua_ls, lua DAP
- `editor/shell/` - bash LSP
- `ui/` - colorscheme, lualine, bufferline, noice, which-key, yazi
- `navigation/` - neo-tree, harpoon, buffers, tmux navigation
- `snacks/` - Snacks.nvim config (picker, explorer, dashboard, zen mode, images, scratch)
- `overseer/` - Task runner with custom templates per language
- `luasnip/` - Snippet engine + custom snippets per language (heavy Java/Spring/MapStruct/JUnit snippets)
- `archive/` - Disabled/replaced plugins kept for reference (do not import these)

### Custom Modules (`lua/modules/`)

Complex features extracted into standalone modules:
- `java/mapstruct/` - MapStruct `@Mapping` path completion: runs a Java server (IPC via Unix socket) that explores MapStruct type hierarchies. Integrates as a blink.cmp source (`modules.blink.mapstruct-source`)
- `java/junit/` - Custom JUnit test runner: builds `junit-platform-console-standalone` commands with classpath from JDTLS, supports single test/file/package/parametrized
- `java/test-report/` - JUnit XML report parser, shows test diagnostics in Trouble
- `java/refactor/` - Java refactoring helpers (import fixing after file moves, sibling usage fixer)

### Utilities (`lua/utils/`)

Shared helpers used across the config:
- `java/java-common.lua` - Java project detection (`is_java_project()`), class-to-path resolution, project type (Maven/Gradle), Java binary paths (SDKMAN)
- `java/jdtls-util.lua` - JDTLS-specific helpers: JDT link extraction/navigation, hover markdown link conversion, find implementations
- `java/jdtls-classpath-util.lua` - Get classpath from running JDTLS instance
- `java/java-ts-util.lua` - Treesitter queries for Java (get class name, method signature, package)
- `java/java-trace.lua` - Parse Java stack traces to quickfix list, highlight traces in buffers
- `java/maven-util.lua`, `maven-compile.lua` - Maven build integration
- `java/javap-util.lua` - Resolve parametrized method signatures via `javap`
- `project-util.lua` - Multi-file project detection (controls Neo-tree/session auto-open)
- `lsp-util.lua` - Code action helpers (apply by title, toggle between action pairs)
- `common-util.lua`, `string-util.lua`, `list-util.lua`, `buffer-util.lua` - General utilities

### Libraries (`lua/lib/`)

Pure utility code: XML parser, file operations, memoization.

### Globals

- `_G.global.dotfiles_path(rel)` - Resolves path relative to `~/dotfiles/`
- `_G.dd(...)` - Debug inspect via Snacks
- `_G.task.test_type` / `_G.task.run_type` - Enums for Overseer test/run commands

## Key Design Patterns

- **Conditional Java loading**: Many plugins/keymaps use `java_util.is_java_project()` to only activate in Java projects. This checks for Maven/Gradle build files in the project root
- **Outside-file guard**: `java_util.if_java_file_outside()` prevents LSP attachment for Java files outside the current working directory (important for multi-microservice setups)
- **LSP hover filtering**: `lsp.lua` wraps `vim.lsp.buf_request_all` to filter empty hover results and convert JDTLS markdown links
- **Diagnostics filtering**: Generated sources (`target/generated-sources/`, `build/generated/`) only show Error-severity diagnostics from JDTLS
- **Overseer task templates**: Language-specific runners in `plugins/overseer/tasks/lang/` resolved by filetype via `lang-runner-resolver`
- **Picker**: Uses Snacks picker (not Telescope/fzf). Set via `vim.g.lazyvim_picker = "snacks"`
- **Session**: resession.nvim with scope.nvim (replaces LazyVim's persistence.nvim). Auto-saves per-directory sessions on exit, restores on startup for multi-file projects
- **Completion**: blink.cmp with custom MapStruct source. LSP fallbacks disabled. DAP REPL completion enabled
