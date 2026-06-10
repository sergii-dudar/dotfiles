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

Built on [LazyVim](https://www.lazyvim.org/) with heavy customization. The config is
**language-agnostic by construction**: "primary" languages (currently **Java** and
**Rust**) get full, IDE-level, per-project-isolated tooling, while many other languages
are supported out of the box for run/test/debug.

### Language-agnostic design (primary vs supported)

- **Primary** (Java, Rust) — first-class, IDE-level, and **isolated per project type**
  (a Java project never loads Rust tooling and vice-versa). Each has a gated editor
  config (`plugins/editor/<lang>/`), its own LSP code-actions (`utils/lang/<lang>/`),
  and the shared `<leader>j…` keymap namespace.
- **Supported** (Go, Python, Bash, Lua, C#, JS/TS, C/C++) — run/test/debug/report + LSP,
  but not gated/isolated.
- Add a language via small registries (one entry each): `utils/lang/lang-project.lua`
  (detection), `config/lazy.lua` (gated import), `lang-runner-resolver.lua` (runner),
  `test-report-dispatcher.lua` (report adapter).
- Full model + worked **Go** example: `nvim/.config/nvim/REGISTERING_NEW_MAIN_LANG_INFO.md`.

### Directory Layout (`nvim/.config/nvim/`)

```
init.lua              — Entry point: sets up global debug helpers (_G.dd, _G.bt), loads global-util, bootstraps config.lazy
lua/
  config/             — LazyVim core config (options, keymaps, autocmds, lazy.lua)
  plugins/            — Plugin specs organized by category (see below)
  modules/            — Per-language feature modules (test runners/reports, blink sources, Java tooling) + shared test-report core
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
- `editor/java/` — JDTLS config, immutable generation, lemminx XML *(loaded only in Java projects)*
- `editor/rust/` — rustaceanvim + rust-analyzer keymaps *(loaded only in Rust projects)*
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

Custom feature implementations. Java has the deepest tooling, but per-language test/run modules and a shared test-report core live here too:

- `modules/blink/mapstruct-source/` — Custom blink.cmp completion source for MapStruct `@Mapping` annotations. Only activates for `*Mapper.java` files.
- `modules/java/junit/` — JUnit test runner using `junit-platform-console-standalone`. Supports test types: ALL_TESTS, FILE_TESTS, CURRENT_TEST, CURRENT_PARAMETRIZED_NUM_TEST, ALL_DIR_TESTS. Resolves parametrized test signatures via `javap`.
- `modules/java/mapstruct/` — Spawns an isolated Java IPC server (`mapstruct-path-explorer.jar`) for exploring field paths, types, and enum values in MapStruct mappers.
- `modules/java/refactor/` — Auto-fixes imports (`import-fixer.lua`) and sibling references (`sibling-usage-fixer.lua`) after file renames.
- `modules/java/test-report/` — Java adapter: parses JUnit XML into diagnostics/signs/marks; registers with the generic core below.
- `modules/common/test-report/` — **generic, filetype-agnostic test-report core** (parse → signs / diagnostics / output panel / tree); language adapters register with it.
- `modules/<lang>/` — per-language test runners + report adapters: `rust/` (cargo-nextest), `go/` (`go test -json`), `python/` (pytest), `bash/` (bashunit), `lua/` (busted), `cs/`, `js/`.

### Utilities (`lua/utils/`)

Shared helper libraries following the `local M = {} ... return M` module pattern:

- `global-util.lua` — Defines `_G.global` namespace (e.g., `dotfiles_path()`)
- `constants.lua` — Shared constants + per-language tables (`M.java`, `M.rust`, `M.go`, …)
- `common-util.lua`, `string-util.lua`, `list-util.lua` — General-purpose helpers
- `logging-util.lua` — Configurable logging with file output
- `lsp-util.lua` — LSP client lookup (`get_client_by_name` / `get_clients_by_name`) + generic code-action helpers
- `nio-util.lua` — Async wrappers using nvim-nio
- `cache-util.lua` — Caching mechanism
- `envs-util.lua` — Environment variable loading
- `utils/java/` — Extensive Java tooling: JDTLS integration, classpath resolution, treesitter-based Java parsing, Maven utilities, javap bytecode parsing, project name resolution
- `utils/lang/` — **language registry layer**: `lang-project.lua` (project/lang detection, `M.current()`/`M.is()`), `lsp-common.lua` (generic code-action apply), `lsp-lang-handlers-resolver.lua` (loads a language's custom LSP handlers if any — only Java registers them today), `java/` & `rust/` (per-language LSP code-action data + handlers)

### Overseer Task System (`lua/plugins/overseer/` + `lua/overseer/`)

Task runner framework for build/test/debug/run operations:

- Global `_G.task` namespace defines test type and run type constants
- Per-language runners: Java, Python, Go, Rust, and more
- `lang-runner-resolver.lua` — Detects language and selects appropriate runner
- Custom components: DAP console control, per-language test-report parsing (JUnit XML, cargo/nextest, `go test` JSON, pytest, …) → diagnostics
- `test-report-dispatcher.lua` routes test-output/diagnostics keymaps to the active filetype's adapter (`modules/<lang>/test-report` + `modules/common/test-report`)

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
- **Preserve existing code**: When making changes, do not remove unused methods/functions, comments, or commented-out code unless explicitly asked to. Keep dead-but-intentional code in place (this complements the Archive pattern); refactors should migrate such code, not silently delete it.
