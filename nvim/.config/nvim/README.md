# My Neovim config

### Screenshots

![neovim.png](../../../screenshots/nvim/neovim.png)

![neovim-grep.png](../../../screenshots/nvim/neovim-grep.png)

A [LazyVim](https://www.lazyvim.org/)-based setup tuned for **IDE-level** work in a few
main languages (Java first — moving off IntelliJ — then Rust), while staying
language-agnostic so new languages plug in cleanly.

## Stack

- **Base:** LazyVim + lazy.nvim · leader = `Space`
- **UI:** gruvbox-material, lualine, bufferline, noice, which-key, [snacks.nvim](https://github.com/folke/snacks.nvim) (picker / explorer / dashboard / zen), yazi
- **Editing / LSP:** nvim-lspconfig + mason, **blink.cmp** completion, conform.nvim (format), nvim-lint, treesitter, trouble.nvim
- **Debug / tasks:** nvim-dap, **overseer.nvim** (per-language run / test / debug)
- **Navigation:** neo-tree, harpoon2, tmux-navigator
- **Snippets:** LuaSnip · **Sessions:** resession.nvim + scope.nvim · **AI:** sidekick

## Languages

**Primary** (IDE-level, isolated per project type):

- **Java** — jdtls + Spring Boot LS, with custom MapStruct `@Mapping` completion, a
  JUnit runner, import/sibling refactor fixers on rename, a dependency-source browser,
  hover-link rewriting and argument-mismatch highlighting.
- **Rust** — rustaceanvim / rust-analyzer + cargo-nextest test reports.

**Supported out of the box** (run / test / debug / report + LSP): Go, Python, Bash,
Lua, C#, JavaScript / TypeScript, C / C++.

> What "primary vs supported" means and how to register a new main language (with a
> worked **Go** example): see
> [`REGISTERING_NEW_MAIN_LANG_INFO.md`](./REGISTERING_NEW_MAIN_LANG_INFO.md).

Custom per-language tooling lives in `lua/modules/<lang>/`, run/test glue in
`lua/plugins/overseer/`, and detection / LSP / keymaps behind small registries in
`lua/utils/lang/`. Deeper architecture notes: [`CLAUDE.md`](./CLAUDE.md).

## Install (Linux / macOS)

Managed with [GNU Stow](https://www.gnu.org/software/stow/); the repo **must** live at
`~/dotfiles`.

```sh
# 1. clone the dotfiles repo to ~/dotfiles
git clone https://github.com/sergii-dudar/dotfiles.git ~/dotfiles

# 2. symlink the nvim config -> ~/.config/nvim
cd ~/dotfiles
stow nvim

# 3. launch — lazy.nvim bootstraps itself and installs everything
nvim
```

On first launch lazy.nvim installs the plugins and **mason** installs the LSPs / DAP
adapters / formatters. Use `:Lazy` and `:Mason` to check status and `:checkhealth` to
diagnose. Remove with `cd ~/dotfiles && stow -D nvim`.

### Requirements

- **Neovim ≥ 0.10** (0.11+ recommended)
- `git`, [`ripgrep`](https://github.com/BurntSushi/ripgrep), [`fd`](https://github.com/sharkdp/fd), a **Nerd Font**, and a C compiler (for treesitter)
- **GNU Stow**
- Per-language toolchains as needed: a JDK (e.g. via SDKMAN) for Java, `rustup` for
  Rust, `go`, Python, Node, … — test/debug extras are listed in each
  `lua/modules/<lang>/*.md`.
