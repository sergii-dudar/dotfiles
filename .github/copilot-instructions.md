# Copilot Instructions — Dotfiles Repository

This is a personal dotfiles repository managed with [GNU Stow](https://www.gnu.org/software/stow/). Each top-level directory follows the stow convention: `<app>/<path-from-home>/`. For example, `nvim/.config/nvim` symlinks to `~/.config/nvim` via `stow nvim`. The repo must live at `~/dotfiles`.

The primary and most complex component is the **Neovim configuration** (`nvim/.config/nvim/`). Most instructions below focus on it.

## Formatting

Lua files use [StyLua](https://github.com/JohnnyMorganz/StyLua) with project config in `nvim/.config/nvim/stylua.toml`:

- 4-space indentation (spaces, not tabs)
- 120-character line width
- Double quotes preferred
- Unix line endings

Format: `stylua nvim/.config/nvim/`

## Neovim Configuration Architecture

Built on [LazyVim](https://www.lazyvim.org/) with heavy customization, especially for Java development.

### Directory Layout (`nvim/.config/nvim/`)

```
init.lua              — Entry point: sets up global debug helpers (_G.dd, _G.bt), loads global-util, bootstraps config.lazy
lua/
  config/             — LazyVim core config (options, keymaps, autocmds, lazy.lua)
  plugins/            — Plugin specs organized by category (see below)
  modules/            — Custom feature modules (blink sources, java tooling)
  utils/              — Shared utility libraries
  lib/                — Standalone libraries (file ops, memoization, XML parsing)
  overseer/           — Overseer task runner components
after/ftplugin/       — Filetype-specific settings (java.lua is the largest — JDTLS compile keymaps, progress handlers)
snippets/             — VSCode-format JSON snippets
queries/java/         — Custom tree-sitter queries for Java
```

### Plugin Organization (`lua/plugins/`)

Plugin specs follow standard lazy.nvim format. They are grouped into subdirectories by concern:

- `editor/` — LSP, DAP, completion (blink.cmp), formatting, linting, git, docker
- `editor/java/` — JDTLS config, immutable generation, lemminx XML
- `editor/shell/` — Bash LSP and debugging
- `editor/lua/` — Lua DAP and LS
- `navigation/` — Harpoon2, neo-tree, tmux-navigator, buffer management
- `ui/` — Colorscheme (gruvbox-material), lualine, noice, which-key, bufferline
- `snacks/` — Snacks.nvim framework (pickers, dashboard, input)
- `luasnip/` — Snippet engine with per-language Lua snippets (java, rust, bash, etc.)
- `overseer/` — Task runner configs with per-language runners
- `fun/` — Optional fun plugins (hardtime, precognition)
- `archive/` — Disabled/replaced plugins kept for reference

LazyVim extras are configured in `lua/config/lazy.lua` (not in `lazyvim.json`).

### Modules (`lua/modules/`)

Custom feature implementations, primarily for Java:

- `modules/blink/mapstruct-source/` — Custom blink.cmp completion source for MapStruct `@Mapping` annotations. Only activates for `*Mapper.java` files.
- `modules/java/junit/` — JUnit test runner using `junit-platform-console-standalone`. Supports test types: ALL_TESTS, FILE_TESTS, CURRENT_TEST, CURRENT_PARAMETRIZED_NUM_TEST, ALL_DIR_TESTS. Resolves parametrized test signatures via `javap`.
- `modules/java/mapstruct/` — Spawns an isolated Java IPC server (`mapstruct-path-explorer.jar`) for exploring field paths, types, and enum values in MapStruct mappers.
- `modules/java/refactor/` — Auto-fixes imports (`import-fixer.lua`) and sibling references (`sibling-usage-fixer.lua`) after file renames.
- `modules/java/test-report/` — Parses JUnit XML reports into Neovim diagnostics, signs, and marks.

### Utilities (`lua/utils/`)

Shared helper libraries following the `local M = {} ... return M` module pattern:

- `global-util.lua` — Defines `_G.global` namespace (e.g., `dotfiles_path()`)
- `constants.lua` — Shared constants (Maven diagnostic sources, junit constants)
- `common-util.lua`, `string-util.lua`, `list-util.lua` — General-purpose helpers
- `logging-util.lua` — Configurable logging with file output
- `lsp-util.lua` — LSP client management helpers
- `nio-util.lua` — Async wrappers using nvim-nio
- `cache-util.lua` — Caching mechanism
- `envs-util.lua` — Environment variable loading
- `utils/java/` — Extensive Java tooling (18 files): JDTLS integration, classpath resolution, treesitter-based Java parsing, Maven utilities, javap bytecode parsing, project name resolution

### Overseer Task System (`lua/plugins/overseer/` + `lua/overseer/`)

Task runner framework for build/test/debug/run operations:

- Global `_G.task` namespace defines test type and run type constants
- Per-language runners: Java, Python, Go, Rust, and more
- `lang-runner-resolver.lua` — Detects language and selects appropriate runner
- Custom components: DAP console control, JUnit XML report parsing with diagnostics

### Key Global Namespaces

- `_G.dd(...)` — Debug dump via `Snacks.debug.inspect`
- `_G.bt()` — Debug backtrace via `Snacks.debug.backtrace`
- `_G.global` — Dotfiles utilities (e.g., `global.dotfiles_path()`)
- `_G.task` — Overseer task type constants (test types, run types)

## Code Conventions

- **Module pattern**: All Lua modules use `local M = {} ... return M`
- **Requires at top**: Dependencies are `require()`-d at the top of the file, assigned to local variables
- **Plugin specs**: One file per plugin (or logical group), returning a table of spec(s)
- **Error handling**: `pcall()` for safe requires, `vim.notify()` for user feedback with severity levels
- **Async**: Use `nio_util.run()` for non-blocking operations, `vim.schedule()` for UI updates
- **Indentation**: 4 spaces universally (Lua, Java, most filetypes). JSON/YAML/SQL use 2 spaces (see `.editorconfig`).
- **StyLua directives**: `-- stylua: ignore start/end` when formatting must be suppressed (e.g., alignment tables)
- **Archive pattern**: Disabled code goes to `archive/` subdirectories rather than being deleted
