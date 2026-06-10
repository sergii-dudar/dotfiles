# Repository Guidelines

## Project Structure & Module Organization

This is a LazyVim-based Neovim configuration. `init.lua` loads shared globals and bootstraps `lua/config/lazy.lua`. Core settings live in `lua/config/`: options, keymaps, autocmds, and plugin imports. Plugin specs are grouped under `lua/plugins/` by purpose, such as `editor/`, `ui/`, `navigation/`, `overseer/`, `snacks/`, and `tools/`.

Custom language features live in `lua/modules/<lang>/`, with shared test-report infrastructure in `lua/modules/common/test-report/`. General helpers are in `lua/utils/`; Java-specific helpers are in `lua/utils/java/`, and language detection/LSP helpers are in `lua/utils/lang/`. Snippets live in `lua/plugins/luasnip/snippets/` and `snippets/`. Disabled experiments are kept in `lua/plugins/archive/`.

## Build, Test, and Development Commands

- `nvim`: start Neovim; lazy.nvim bootstraps plugins on first launch.
- `:Lazy`: inspect, sync, update, or profile plugins.
- `:Mason`: inspect LSP, DAP, formatter, and linter tools.
- `:checkhealth`: diagnose Neovim, plugin, and provider problems.
- `stylua .`: format all Lua files using this repo's `stylua.toml`.
- `stylua --check .`: verify Lua formatting without rewriting files.
- `nvim --headless "+Lazy! sync" +qa`: sync plugins in a noninteractive session.

## Coding Style & Naming Conventions

Lua uses StyLua with 4-space indentation, Unix line endings, 120 columns, and double quotes preferred where StyLua can choose. Prefer small modules that return a table named `M`. Name files and directories in lowercase kebab case, for example `test-report/`, `lsp-java.lua`, and `java-ts-util.lua`.

Keep primary-language configuration isolated behind `utils.lang.lang-project` checks. Do not import Java-only or Rust-only editor config globally. Preserve intentional archived, commented, or currently-unused code unless the task explicitly asks for removal.

## Testing Guidelines

There is no single repository-wide test runner. Validate config changes with `nvim --headless "+checkhealth" +qa` when possible, and smoke-test affected commands interactively. For language modules, use local docs such as `lua/modules/rust/RUST_TESTS.md`, `lua/modules/go/GO_TESTS.md`, or `lua/modules/cs/CS_TESTS.md`.

For Lua formatting changes, run `stylua --check .`. For plugin changes, open `:Lazy` and confirm specs load without errors.

## Commit & Pull Request Guidelines

Recent commits use a scoped style like `nvim, lsp: refactoring to make language agnostic`. Follow `area, subsystem: concise summary`, for example `nvim, java: fix junit report parsing`.

Pull requests should include a short description, the affected language or plugin area, manual validation steps, and screenshots only for visible UI changes. Link related issues or notes when changing documented architecture.

## Agent-Specific Instructions

Respect user work in progress. Check `git status --short` before edits, avoid reverting unrelated changes, and keep edits scoped to the requested behavior.
