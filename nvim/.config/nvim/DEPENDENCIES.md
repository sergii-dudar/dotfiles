# External dependencies

Everything this Neovim config calls **outside of Neovim**: CLI tools, language runtimes,
toolchains and jars. Plugins (lazy.nvim) and the tools Mason manages are covered at the end;
they install themselves, but they need the runtimes listed here.

Install / check with [`install_dependencies.sh`](./install_dependencies.sh) (macOS + Homebrew,
Arch-based Linux + pacman, with yay or paru for AUR packages). It only installs what is missing:

```sh
./install_dependencies.sh --check            # report only
./install_dependencies.sh --check core,tools # report only these groups
./install_dependencies.sh                    # install every missing group
./install_dependencies.sh --only core,tools  # just these groups
./install_dependencies.sh --skip cs,ai       # everything except these
./install_dependencies.sh --dry-run          # print the commands only
./install_dependencies.sh --groups           # list groups
JAVA_VERSION=25.0.1-amzn ./install_dependencies.sh --only java
```

Paths the config resolves on its own need these directories on `PATH`: `~/.local/bin`,
`~/.cargo/bin`, `~/.luarocks/bin`, `~/go/bin`.

## `core`: required

| Tool | Used by | brew | pacman |
| --- | --- | --- | --- |
| `nvim` ≥ 0.12.3 | the config itself | `neovim` | `neovim` |
| `git` | lazy.nvim bootstrap (`config/lazy.lua`), gitsigns, zdiff, diffview, `git-review-util` | `git` | `git` |
| `curl` | Mason downloads, SDKMAN, kulala.nvim (grammar + kulala-core), plantuml server probe (`after/ftplugin/plantuml.lua`) | `curl` | `curl` |
| `wget` | Java extension installers (`scripts/java/install_*.sh`), Mason fallback | `wget` | `wget` |
| `unzip` | Mason, `-sources.jar` extraction (`modules/java/dependencies-search`), jdtls bundle MANIFEST read (`jdtls-config.lua`) | `unzip` | `unzip` |
| `zip`, `tar`, `gzip` | SDKMAN, Mason archives | `zip` (tar/gzip ship with macOS) | `zip tar gzip` |
| `rg` (ripgrep) | Snacks pickers, grug-far, Java refactor, static-import explorer, project references (`gR`) | `ripgrep` | `ripgrep` |
| `fd` | Snacks pickers, find-dir picker, Java refactor (`buffer-manager`, `mirror-sync`) | `fd` | `fd` |
| `gsed` (GNU sed, macOS only) | Java refactor in-place rewrites (`modules/java/refactor/constants.lua`); BSD sed is incompatible | `gnu-sed` | (`sed` is GNU) |
| `cc`, `make` | treesitter parsers, kulala.nvim tree-sitter grammar | Xcode Command Line Tools | `base-devel` |
| `node`, `npm` | Mason npm packages (bash-ls, prettier, …), `swagger-preview.nvim` build (`npm i`) | `node` | `nodejs npm` |
| `python3` | Mason pip packages (beautysh, sqlfluff, …), `scripts/java/gradle-language-server-stdio` | `python` | `python` |
| `go` | `openapi-language-server` build (`go install .` → `~/go/bin/openapi-language-server`, started from `after/ftplugin/yaml.lua`), gopls / golangci-lint via Mason | `go` | `go` |
| `luarocks` | Mason `luacheck` | `luarocks` | `luarocks` |
| a Nerd Font | icons everywhere | `font-jetbrains-mono-nerd-font` (cask) | `ttf-jetbrains-mono-nerd` |

## `tools`: integrations

| Tool | Used by | brew | pacman |
| --- | --- | --- | --- |
| `lazygit` | Snacks lazygit (`plugins/snacks/init.lua`) | `lazygit` | `lazygit` |
| `yazi` | yazi.nvim (`<leader>yy`) | `yazi` | `yazi` |
| `tmux` | sidekick CLI mux backend (`editor/ai.lua`), tmux navigation, image terminal detection (`snacks/configs/image-terminal.lua`) | `tmux` | `tmux` |
| `jq` | JSON normalize (`utils/json/normalize.lua`), Java `toString` parser | `jq` | `jq` |
| `xxd` | `<leader>Cjb` decode: binary hex dump (`utils/decode-util.lua`) | ships with macOS | `tinyxxd` |
| `xmllint` | XML formatting | ships with macOS | `libxml2` |
| `bat` | class-file decompile previewer (`scripts/java/decompile_stdout.sh`, used by the Snacks pickers) | `bat` | `bat` |
| `rsvg-convert` | snacks.image SVG → PNG for jdtls javadoc diagrams (`snacks/configs/image-refs.lua`) | `librsvg` | `librsvg` |
| `magick` | snacks.image conversions (kitty / ghostty only) | `imagemagick` | `imagemagick` |
| `plantuml` | plantuml.nvim preview server (`plantuml --http-server`) | `plantuml` | `plantuml` (AUR fallback) |
| `mergiraf` | syntax-aware merge driver used with diffview conflict resolution | `mergiraf` | `mergiraf` (AUR fallback) |
| `stow` | symlinking the config (`stow nvim`) | `stow` | `stow` |
| `kubectl` | yaml runner: `kubectl apply -f` (`overseer/tasks/lang/simple-runners.lua`) | `kubernetes-cli` | `kubectl` |

## `java`: primary language

| Tool | Used by | Installed via |
| --- | --- | --- |
| JDK at `~/.sdkman/candidates/java/current` (Java 25+) | jdtls, Spring Boot LS, runners (`java-runner.lua`), JUnit runner, `javap`, MapStruct server, Mason Java tools (google-java-format, ktfmt, ktlint, checkstyle) | SDKMAN: `sdk install java $JAVA_VERSION` |
| `mvn` | `utils/java/maven-compile.lua`, Maven runner | SDKMAN: `sdk install maven` |
| `~/tools/java-extensions/junit/junit-platform-console-standalone.jar` | JUnit runner (`modules/java/junit`) | `scripts/java/install_junit_console_launcher.sh` |
| `~/tools/java-extensions/jmockit/jmockit.jar` | JMockit `-javaagent` for JUnit runs | `scripts/java/install_jmockit_agent.sh` |
| `~/tools/java-extensions/decompiler/fernflower.jar` | class-file decompile preview | `scripts/java/install_fernflower_decompiler.sh` |
| `~/tools/java-extensions/mapstruct/mapstruct-path-explorer-repo/target/mapstruct-path-explorer.jar` | MapStruct `@Mapping` completion server (`modules/java/mapstruct`) | `scripts/java/install_mapstruct_path_explorer_server.sh` |
| `~/tools/java-extensions/lemminx/{lemminx-ls,lemminx-maven}/*.jar` | lemminx + lemminx-maven for `pom.xml` (`editor/lsp-lemminx-maven.lua`) | `scripts/java/lemminx-maven-ls/load_and_build_all.sh` |
| `byte-buddy-agent` in `~/.m2/repository` | JUnit runner agent (latest version found) | any Maven build that pulls Mockito |

The Java extension scripts need `git`, `curl`/`wget` and the JDK (they build with `mvnw` / `gradlew`).

## Other languages (run / test / debug)

| Group | Tool | Used by | brew | pacman |
| --- | --- | --- | --- | --- |
| `rust` | `rustup` + stable toolchain (`cargo`) | runner, `cargo metadata`, rustaceanvim | `rustup` | `rustup` |
| `rust` | `cargo-nextest` | test runner + JUnit XML (`modules/rust/cargo-test`) | `cargo-nextest` | `cargo-nextest` |
| `go` | `dlv` | Go debug + debug test | `delve` | `delve` |
| `python` | `python3.14` | Python runner (hardcoded in `python-runner.lua`) | `python@3.14` | `python` |
| `lua` | `lua` | Lua runner | `lua` | `lua` |
| `lua` | `busted` | Lua test runner (`modules/lua/busted-test`; also finds `~/.luarocks/bin/busted`) | `luarocks install --local busted` | same |
| `js` | `deno` | JS/TS runner and debug (`js-runner.lua`) | `deno` | `deno` |
| `cs` | `dotnet` | `dotnet run` / `dotnet test` (TRX) | `dotnet-sdk` (cask) | `dotnet-sdk` |
| `cs` | `netcoredbg` | C# debug | `:MasonInstall netcoredbg` | `netcoredbg` (AUR) |
| `bash` | `/opt/homebrew/bin/bash` (macOS) | bash runner (hardcoded path; Linux uses `/bin/bash`) | `bash` | - |
| `bash` | `bashunit` | bash test runner (JUnit XML) | installer → `~/.local/bin` | same |
| `c` | `gcc`, `g++` | C / C++ runners (`clang-runner.lua`, `cpp-runner.lua`) | Xcode Command Line Tools | `gcc` |
| `ai` | `copilot` | sidekick.nvim CLI (`current_ai = "copilot"` in `editor/ai.lua`) | `npm i -g @github/copilot` | same (user prefix `~/.local` if global npm is root-owned) |

Per-project test tools are **not** installed globally, so the script leaves them out:

- `pytest`: into the project venv (`pip install pytest`); `debugpy` comes from Mason.
- `jest` (+ `ts-jest typescript` for TS): `npm i -D jest` in the project.

## Installed by Neovim itself

Nothing to do here beyond `core`: these appear on first start.

- **lazy.nvim plugins**, including build steps: `swagger-preview.nvim` (`npm i`) and
  `openapi-language-server` (`go install .`).
- **Mason** `ensure_installed` (`plugins/editor/mason.lua`) plus the LazyVim extras:
  bash-language-server, lemminx, autotools-language-server, gopls, golangci-lint, jdtls,
  vscode-spring-boot-tools, vscode-java-dependency, vscode-java-decompiler,
  gradle-language-server, rust-analyzer, bash-debug-adapter, local-lua-debugger-vscode,
  checkstyle, ktlint, shellcheck, ruff, luacheck, stylelint, xmlformatter,
  google-java-format, ktfmt, beautysh, stylua, shellharden, fourmolu, prettier, prettierd,
  sqlfluff, tree-sitter-cli, lua-language-server (+ java-debug-adapter, java-test, codelldb,
  debugpy, js-debug-adapter, … from the extras).
  Runtimes they need: `node`/`npm`, `python3`, `go`, `luarocks`, JDK, `curl`/`wget`, `unzip`, `tar`, `gzip`.
  `beautysh` also needs `setuptools` in its venv:
  `~/.local/share/nvim/mason/packages/beautysh/venv/bin/python -m pip install setuptools`.
- **Treesitter parsers**: compiled with `cc` (and `tree-sitter-cli` from Mason).

## Manual / per-machine

- **kulala-core** (`plugins/tools/kulala.lua`): kulala.nvim downloads it on macOS (and `codesign`s
  it, see `utils/kulala-core-util.lua`). Linux has no release, so build it with `bun` and point
  `opts.kulala_core.path` at the binary. The steps are in the comment at the top of `kulala.lua`.
- **Graphics terminal** for snacks.image: kitty or ghostty (`snacks/configs/image-terminal.lua`).
- **HTML runner** opens the file in a browser (`simple-runners.lua`, falls back to Brave on macOS).

## Not required (commented out / archived)

Referenced only in commented-out code or `lua/plugins/archive/`: `fortune`, `cowsay`,
`fastfetch` (dashboard), `kulala-fmt`, `ronfmt`, `~/dotfiles/work/formatter/spotless.sh`,
`haskell-language-server`, `stack` / `runhaskell` (Haskell runner, extra not enabled), and the VS Code
`vscode-java-debug` / `vscode-java-test` bundles (Mason's java-debug-adapter / java-test are used instead).
