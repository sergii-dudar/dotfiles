# AGENTS.md

This file provides guidance to Codex (Codex.ai/code) when working with code in this repository.

## Repository purpose

Personal dotfiles for Arch Linux, Ubuntu/Debian, and macOS workstations. Symlinks are managed by [GNU Stow](https://www.gnu.org/software/stow/).

## Hard environmental requirements

These are baked into scripts and configs throughout the repo — changing them tends to break things in non-obvious places:

- The repo MUST be cloned at `~/dotfiles`. Many scripts and Neovim modules resolve paths via `~/dotfiles/...` (e.g. `_G.global.dotfiles_path()` in the nvim config).
- Wallpapers are expected at `~/wallpapers` (some terminal/WM configs reference them).
- Top-level directories follow stow convention: `<app>/<path-relative-to-$HOME>/`. For example `nvim/.config/nvim/` becomes `~/.config/nvim` after `stow nvim`. Preserve this layout when adding new app configs.

## Formatting & style

- Lua is formatted with **StyLua**. The config lives at `nvim/.config/nvim/stylua.toml` (4-space indent, 120 col, double quotes). Run `stylua nvim/.config/nvim/` to format the Neovim config.
- `.editorconfig` is the source of truth for indent rules: 4 spaces by default, 2 spaces for JSON/YAML/SQL/feature files, Java max line 150 with custom import layout (`*, |, java.**, javax.**, |, $*`).
- Line endings are LF, no trailing newline (`insert_final_newline = false`).
- For repository text searches, prefer `grep` over `rg`/ripgrep unless a task-specific constraint requires otherwise.

## Neovim configuration

The largest and most complex component. It has its own dedicated guidance files — read them before touching anything under `nvim/.config/nvim/`:

- `nvim/.config/nvim/AGENTS.md` — full architecture (entry point, plugin grouping, custom per-language modules under `lua/modules/`, the `utils/lang/` registries, `_G.global` / `_G.task` / `_G.dd` globals, picker/session/completion design choices).
- `nvim/.config/nvim/REGISTERING_NEW_MAIN_LANG_INFO.md` — primary vs supported languages and how to register a new main language (worked Go example).
- `nvim/.config/nvim/README.md` — user-facing stack overview + install.

High-level: LazyVim base + lazy.nvim, **language-agnostic by construction** — a project's tooling loads only for its language. **Primary** languages (IDE-level, per-project isolated) are **Java** (JDTLS + Spring; custom JUnit runner, MapStruct completion via an out-of-process Java IPC server, dependency-source picker, refactor helpers) and **Rust** (rustaceanvim). Many others (Go, Python, Bash, Lua, C#, JS/TS, C/C++) are **supported** for run/test/debug/report. Detection + gating run through `lua/utils/lang/lang-project.lua`; test reports through a shared `lua/modules/common/test-report` core + per-language adapters. Snacks.nvim is the picker/UI framework (not Telescope/fzf). Sessions use resession.nvim + scope.nvim (LazyVim's `persistence.nvim` is replaced).

Disabled/replaced plugins live in `lua/plugins/archive/` — do **not** import from there in `lua/config/lazy.lua`.

## Other notable trees

- `bin/` — helper scripts grouped by topic (`install/`, `java/`, `macos/`, `wmscripts/`, `tmux/`, etc.). Many are sourced/invoked by WM and shell configs; before renaming or moving one, grep for its basename across the repo.
- `tmux/` + `tmux-powerline/` + `sesh/` — tmux session/multiplexer stack. `sesh` has custom session-management built on top of the tmux API.
- `keyboard/kanata/{linux,macos}/` — current keyboard remapping configs. `nonhome/keyd` and `nonhome/kmonad` are previous-generation configs kept for reference; `karabiner/` is the previous macOS setup.
- `idea/.ideavimrc` — IntelliJ IdeaVim config, kept aligned with Neovim keymaps.
- `cron/scripts/` — cronie jobs invoked from user crontab.
- `temp/`, `screenshots/`, `information/`, `installed-packages/` — non-config working directories. Check the dir's own README before assuming purpose.

## `gh-push.sh` — Zscaler workaround

`./gh-push.sh [branch]` replays local commits to GitHub via the Git Data REST API instead of the HTTPS pack protocol (which is blocked by Zscaler in the user's work environment). It rebuilds blobs, trees, and commits one at a time, preserving author/committer/timestamps so resulting remote SHAs match local SHAs.

- Requires `TOKEN` env var: `TOKEN=$(GH_HOST=github.com gh auth token) ./gh-push.sh`.
- Branch defaults to current; `origin` must point at GitHub; `origin/HEAD` (or `main`) is used as the base for new branches.
- Merge commits are flattened to first-parent diffs — be aware if you need to push history with non-linear merges intact.

Use this instead of `git push` when on the work network. Do not modify it casually; it is the only working push path in that environment.

## When making changes

- For Lua under `nvim/.config/nvim/`, run `stylua` before considering work done.
- **Preserve existing code**: do not remove unused methods/functions, comments, or commented-out code unless explicitly asked to. Keep dead-but-intentional code in place (complements the `archive/` pattern); refactors should migrate such code, not silently delete it.
- New app configs should follow the stow layout. Don't introduce hardcoded absolute paths outside `~/dotfiles` and `~/.config`.
- Other tool-specific guidance lives in per-app `README.md` files (see `nvim/.config/nvim/README.md`, `tmux/README.md`, `aerospace/.config/aerospace/README.md`, `hyprland/README.md`, etc.) — consult them before changing the corresponding config.
