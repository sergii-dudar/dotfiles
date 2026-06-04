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

Plugin specs are imported in `lua/config/lazy.lua`. Imports actually wired:
`plugins.ui`, `plugins.navigation`, `plugins.editor`, `plugins.editor.java`,
`plugins.editor.shell`, `plugins.editor.lua`, `plugins.snacks`, `plugins.tools`,
plus a catch-all `plugins` import that picks up subdirs with `init.lua`
(`overseer/`, `luasnip/`).

- `editor/` — LSP (`lsp.lua`, `lsp-yaml.lua`, `lsp-servers.lua`, `mason.lua`), completion (`blink-cmp.lua`), formatting (`conform.nvim.lua`), DAP (`nvim-dap.lua`), treesitter, git, linting, AI (`ai.lua`, sidekick), session, trouble
- `editor/java/` — JDTLS, Spring Boot LS, java-deps, Java file rename with package update
- `editor/lua/` — lua_ls, lua DAP
- `editor/shell/` — bash LSP
- `ui/` — colorscheme, lualine, bufferline, noice, which-key, yazi
- `navigation/` — neo-tree, harpoon, buffers, tmux navigation
- `snacks/` — Snacks.nvim config (picker, explorer, dashboard, zen mode, images, scratch)
- `tools/` — standalone tool integrations (currently `kulala.lua`)
- `overseer/` — Task runner, templates per language under `tasks/lang/`, plus `overseer-util.lua`, `overseer-task-util.lua`, `test-report-dispatcher.lua`
- `luasnip/` — Snippet engine + custom snippets per language (heavy Java/Spring/MapStruct/JUnit snippets)
- `fun/` — extra "fun" plugins (drop-nvim, hardtime, precognition, meow.yarn). **Currently commented out in `lazy.lua`** — not loaded
- `archive/` — Disabled/replaced plugins kept for reference (do not import these)

### Custom Overseer Components (`lua/overseer/component/`)

Separate from `lua/plugins/overseer/`. Holds custom Overseer component
definitions registered at runtime: `debug/` (DAP integration) and
`test_report/` (hands JUnit/cargo/go/pytest/busted/bashunit XML output to the
matching `modules/<lang>/test-report` adapter via `test-report-dispatcher.lua`).

### Custom Modules (`lua/modules/`)

Complex features extracted into standalone modules. Per-language test runners
share a `<lang>/test-report/` adapter that conforms to the same `LangAdapter`
contract used by `modules/common/test-report` (parses test result XML/JSON,
emits diagnostics + signs into the buffer, opens Trouble view).

- `java/mapstruct/` — MapStruct `@Mapping` path completion engine. Runs an
  out-of-process Java server (`mapstruct-path-explorer.jar`) over a Unix
  socket and explores type hierarchies. Exposed as a blink.cmp source via
  `modules/blink/mapstruct-source/` (the source is the thin adapter; the
  engine lives here)
- `java/junit/` — Custom JUnit test runner: builds
  `junit-platform-console-standalone` commands with classpath from JDTLS,
  supports single test/file/package/parametrized iterations, integrates
  `:ParamTestNum` and `task.last_test`
- `java/test-report/` — JUnit XML report parser, writes diagnostics + signs,
  auto-opens Trouble (`junit_diagnostics`), tree sidebar, scratch
  stdout/stderr/stacktrace split
- `java/refactor/` — Batch fixer for moved/renamed Java files & packages
  (ripgrep + GNU sed; `import-fixer`, `sibling-usage-fixer`)
- `java/dependencies-search/` — Search/browse dependency source code via
  Snacks picker. Extracts `-sources.jar` in `~/.m2/repository/`, provides
  files/grep/explorer pickers with jdtls-aware file opening. Keymaps:
  `<leader>jdl` (load), `<leader>j.` (files), `<leader>j/` (grep),
  `<leader>je` (explore). `<C-a>` toggle all/filtered, `<C-s>` single-module,
  `<C-o>` jdtls vs raw opener
- `java/static-import-explorer/` — ripgrep + Snacks-picker fallback for
  `import static` when JDTLS doesn't surface the code action
- `bash/`, `go/`, `lua/`, `python/`, `rust/` — language adapters, each with
  a `<framework>-test/` runner + `test-report/` parser
  (bashunit / cargo / busted / pytest / go test). Per-language docs live in
  `README.md` / `*_TESTS.md` next to the module
- `common/test-report/` — shared test-report infrastructure used by all
  language adapters (tree view, diagnostic namespace, Trouble registration)
- `blink/mapstruct-source/` — blink.cmp source adapter for the
  `java/mapstruct` engine

### Utilities (`lua/utils/`)

Shared helpers used across the config. Java-specific helpers live under
`utils/java/`; the rest is general-purpose. Notable subdirs: `nvim/` (Neovim
internals helpers), `tests/` (test-runner utilities), `ui/` (UI helpers),
`linter/`, `json/`, `archive/`. Top-level files include:

- `project-util.lua` — Multi-file project detection (controls Neo-tree/session auto-open)
- `lsp-util.lua` — Code action helpers (apply by title, toggle between action pairs)
- `dap-util.lua` — DAP helpers (clipboard copy of evaluated values, etc.)
- `logging-util.lua`, `cache-util.lua`, `nio-util.lua` — infrastructure
- `resource-cwd-resolver.lua` — resolve resources relative to project root
- `indent-util.lua`, `buffer-util.lua`, `common-util.lua`, `string-util.lua`,
  `list-util.lua`, `diff-util.lua`, `envs-util.lua`, `neotree-util.lua` — general
- `global-util.lua` — sets `_G.global` (loaded from `init.lua`)
- `constants.lua` — shared constants

Java helpers under `utils/java/`:

- `java-common.lua` — Java project detection (`is_java_project()`), class-to-path resolution, project type (Maven/Gradle), Java binary paths (SDKMAN)
- `jdtls-util.lua` — JDTLS-specific: JDT link extraction/navigation, hover markdown link conversion, find implementations
- `jdtls-classpath-util.lua` — Get classpath from running JDTLS instance
- `java-ts-util.lua` — Treesitter queries for Java (class name, method signature, package)
- `java-trace.lua` — Parse Java stack traces to quickfix list, highlight traces in buffers
- `maven-util.lua`, `maven-compile.lua` — Maven build integration
- `javap-util.lua` — Resolve parametrized method signatures via `javap`

### Libraries (`lua/lib/`)

Pure utility code: XML parser, file operations, memoization.

### Per-project LSP overrides

`.neoconf.json` at the config root holds project-scoped LSP/server overrides
(via folke/neoconf.nvim). Edit it for per-project settings rather than
hardcoding into plugin specs.

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
