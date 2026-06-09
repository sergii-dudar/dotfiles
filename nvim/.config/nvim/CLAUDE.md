# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

Personal Neovim configuration built on **LazyVim** (folke/LazyVim) distribution with **lazy.nvim** plugin manager. **Language-agnostic by construction**: "primary" languages (currently **Java** + Spring/JDTLS and **Rust**) get full IDE-level, per-project-isolated tooling, while Go/Python/Bash/Lua/C#/JS-TS/C-C++ are supported out of the box for run/test/debug. See `REGISTERING_NEW_MAIN_LANG_INFO.md` for the primary-vs-supported model and how to register a new main language. Part of a larger dotfiles repo (stow-managed).

## Formatting

Lua files are formatted with **StyLua**: 4-space indentation, 120-column width, double quotes preferred. Config is in `stylua.toml`. Run `stylua .` to format.

## Editing conventions

- **Preserve existing code**: do not remove unused methods/functions, comments, or commented-out code unless explicitly asked to. Keep dead-but-intentional code in place (complements the `archive/` pattern); refactors should migrate such code, not silently delete it.

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
`plugins.ui`, `plugins.navigation`, `plugins.editor`, `plugins.editor.shell`,
`plugins.editor.lua`, `plugins.snacks`, `plugins.tools`, plus a catch-all `plugins`
import that picks up subdirs with `init.lua` (`overseer/`, `luasnip/`). The
per-language editor groups `plugins.editor.java` and `plugins.editor.rust` are imported
**conditionally** — `{ import = …, cond = require("utils.lang.lang-project").is("<lang>") }`
— so a project only loads its own language's editor config.

- `editor/` — LSP (`lsp.lua`, `lsp-yaml.lua`, `lsp-servers.lua`, `mason.lua`), completion (`blink-cmp.lua`), formatting (`conform.nvim.lua`), DAP (`nvim-dap.lua`), treesitter, git, linting, AI (`ai.lua`, sidekick), session, trouble
- `editor/java/` — JDTLS, Spring Boot LS, java-deps, Java file rename with package update *(loaded only in Java projects)*
- `editor/rust/` — rustaceanvim + rust-analyzer keymaps/code-actions *(loaded only in Rust projects)*
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

Shared helpers used across the config. The **language registry layer** lives under
`utils/lang/`; Java-specific helpers under `utils/java/`; the rest is general-purpose.
Notable subdirs: `lang/` (language registries — see below), `nvim/` (Neovim internals
helpers), `tests/` (test-runner utilities), `ui/` (UI helpers), `linter/`, `json/`,
`archive/`. Top-level files include:

- `project-util.lua` — Multi-file project detection (controls Neo-tree/session auto-open)
- `lsp-util.lua` — LSP client lookup (`get_client_by_name` / `get_clients_by_name`) + code-action helpers (apply by title, toggle between action pairs)
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

Language registry layer under `utils/lang/`:

- `lang-project.lua` — generic project/language **detection** (root markers + source exts), `M.current()` / `M.is(lang)`; the modern replacement for `java_util.is_java_project()` and the gate used by `config/lazy.lua`
- `lsp-common.lua` — generic `apply_lsp_action` (code-action resolve / edit / command execution)
- `lsp-land-handlers-resolver.lua` — loads a language's custom LSP handlers for the active project (only Java registers any today; Rust/others register none)
- `java/lsp-java.lua`, `rust/lsp-rust.lua` — per-language code-action match-name data + (Java) import-resolve flow
- `java/lsp-java-handlers.lua` — Java-only global LSP handler overrides (jdtls hover-link conversion + diagnostics post-processing), installed only in Java projects

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

- **Per-language isolation**: each primary language's editor config is imported only for its project type via `{ import = …, cond = require("utils.lang.lang-project").is("<lang>") }` in `lazy.lua` — a Java project never loads Rust tooling and vice-versa. `lang-project.current()`/`is()` is the generic detector (root markers + exts); `java_util.is_java_project()` still exists for Java-internal checks
- **Outside-file guard**: `java_util.if_java_file_outside()` prevents LSP attachment for Java files outside the current working directory (important for multi-microservice setups)
- **Per-language LSP handlers**: `lsp.lua` is generic — it calls `utils.lang.lsp-land-handlers-resolver.setup()`, which loads the active language's handler module. Java's (`utils/lang/java/lsp-java-handlers.lua`) wraps `vim.lsp.buf_request_all` to filter empty hover results + convert JDTLS markdown links, and post-processes diagnostics (generated sources under `target/generated-sources/` / `build/generated/` only show Error-severity from JDTLS). Non-Java projects load none of this
- **Overseer task templates**: Language-specific runners in `plugins/overseer/tasks/lang/` resolved by filetype via `lang-runner-resolver`
- **Picker**: Uses Snacks picker (not Telescope/fzf). Set via `vim.g.lazyvim_picker = "snacks"`
- **Session**: resession.nvim with scope.nvim (replaces LazyVim's persistence.nvim). Auto-saves per-directory sessions on exit, restores on startup for multi-file projects
- **Completion**: blink.cmp; the custom MapStruct source is offered only in Java buffers (`per_filetype.java`), and jdtls hover-link conversion is lazy-loaded only for jdtls items. LSP fallbacks disabled. DAP REPL completion enabled
