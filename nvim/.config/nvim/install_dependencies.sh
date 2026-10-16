#!/usr/bin/env bash
# Install the external (outside-Neovim) tools this Neovim config relies on.
# What each tool is used for: DEPENDENCIES.md next to this script.
#
# Supported systems:
#   macOS            - Homebrew (brew formulas + casks)
#   Arch Linux based - pacman, AUR packages through yay or paru
#
# Only missing tools are installed; anything already present is left untouched.
# Tools Mason / lazy.nvim install by themselves (LSPs, DAP adapters, formatters, linters,
# treesitter parsers) are NOT installed here; only the runtimes they need are.
#
# Usage:
#   install_dependencies.sh [options]
#
# Options:
#   --only g1,g2   install only these groups
#   --skip g1,g2   install everything except these groups
#   --check [g1,g2]
#                  only report what is installed / missing, install nothing;
#                  optional groups limit the report (same as --check --only g1,g2)
#   --dry-run      print the commands instead of running them
#   --groups       list the groups and exit
#   -h, --help     show this help
#
# Env:
#   JAVA_VERSION   SDKMAN java identifier (e.g. 25.0.1-amzn); default: SDKMAN default candidate

set -euo pipefail

DOTFILES="$HOME/dotfiles"
JAVA_EXT_DIR="$HOME/tools/java-extensions"
SDKMAN_DIR="${SDKMAN_DIR:-$HOME/.sdkman}"
MASON_BIN="${XDG_DATA_HOME:-$HOME/.local/share}/nvim/mason/bin"
MIN_NVIM_VERSION="0.12.3"

ALL_GROUPS=(core tools java rust go python lua js cs bash c ai)

declare -A GROUP_DESC=(
    [core]="required: neovim, git, rg, fd, compilers and runtimes Mason/lazy.nvim need"
    [tools]="integrations: lazygit, yazi, tmux, jq, image/diagram/merge tools"
    [java]="JDK + Maven (SDKMAN), JUnit launcher, MapStruct server, decompiler, lemminx"
    [rust]="rustup toolchain, cargo-nextest"
    [go]="delve debugger"
    [python]="python3.14 (runner)"
    [lua]="lua interpreter, busted"
    [js]="deno (runner)"
    [cs]="dotnet SDK, netcoredbg"
    [bash]="bashunit, Homebrew bash on macOS"
    [c]="gcc / g++ (C/C++ runners)"
    [ai]="GitHub Copilot CLI (sidekick.nvim)"
)

# group|os|check|brew|pacman|description
#   os:     all | mac | arch
#   check:  comma separated alternatives; a command name, a path (~ allowed),
#           glob:<pattern> or font:nerd
#   brew:   formula | cask:<name> | @<function> | - (system provided / not available)
#   pacman: package (falls back to the AUR when not in the repos) | @<function> | -
DEPS="
core|all|nvim|neovim|neovim|Neovim >= ${MIN_NVIM_VERSION}
core|all|git|git|git|lazy.nvim bootstrap, plugins, git tooling
core|all|curl|curl|curl|Mason, SDKMAN, kulala.nvim, plantuml server probe
core|all|wget|wget|wget|Java extension installers, Mason fallback downloader
core|all|unzip|unzip|unzip|Mason, dependency-sources extraction, jdtls MANIFEST read
core|all|zip|zip|zip|SDKMAN
core|all|tar|-|tar|Mason archives
core|all|gzip|-|gzip|Mason archives
core|all|rg|ripgrep|ripgrep|Snacks pickers, grug-far, Java refactor / static imports / references
core|all|fd|fd|fd|Snacks pickers, find-dir picker, Java refactor
core|mac|gsed|gnu-sed|-|Java refactor (GNU sed is required, BSD sed is not compatible)
core|all|make|-|make|treesitter / plugin builds
core|all|cc|@xcode_clt|base-devel|C compiler for treesitter parsers and kulala grammar
core|all|node|node|nodejs|Mason npm packages, swagger-preview build
core|arch|npm|-|npm|Mason npm packages, swagger-preview build
core|all|python3|python|python|Mason pip packages, gradle-language-server-stdio adapter
core|all|go|go|go|openapi-language-server build (go install), gopls via Mason
core|all|luarocks|luarocks|luarocks|Mason luacheck
core|all|font:nerd|cask:font-jetbrains-mono-nerd-font|ttf-jetbrains-mono-nerd|Nerd Font for icons
tools|all|lazygit|lazygit|lazygit|Snacks lazygit
tools|all|yazi|yazi|yazi|yazi.nvim
tools|all|tmux|tmux|tmux|sidekick CLI mux, tmux navigation, image terminal detection
tools|all|jq|jq|jq|JSON normalize, Java toString parser
tools|all|xxd|-|tinyxxd|base64/JWT decode hex dump
tools|all|xmllint|-|libxml2|XML formatting
tools|all|bat|bat|bat|class decompile previewer (scripts/java/decompile_stdout.sh)
tools|all|rsvg-convert|librsvg|librsvg|snacks.image SVG -> PNG (jdtls javadoc diagrams)
tools|all|magick|imagemagick|imagemagick|snacks.image conversions
tools|all|plantuml|plantuml|plantuml|plantuml.nvim preview server
tools|all|mergiraf|mergiraf|mergiraf|syntax aware merge driver used with diffview
tools|all|stow|stow|stow|symlinking the config
tools|all|kubectl|kubernetes-cli|kubectl|yaml runner (kubectl apply)
java|all|~/.sdkman/candidates/java/current/bin/java|@sdk_java|@sdk_java|JDK (jdtls, runners, Mason java tools)
java|all|mvn,~/.sdkman/candidates/maven/current/bin/mvn|@sdk_maven|@sdk_maven|Maven compile / runner
java|all|~/tools/java-extensions/junit/junit-platform-console-standalone.jar|@java_ext_junit|@java_ext_junit|JUnit runner
java|all|~/tools/java-extensions/jmockit/jmockit.jar|@java_ext_jmockit|@java_ext_jmockit|JMockit agent for JUnit runs
java|all|~/tools/java-extensions/decompiler/fernflower.jar|@java_ext_fernflower|@java_ext_fernflower|class decompile previewer
java|all|~/tools/java-extensions/mapstruct/mapstruct-path-explorer-repo/target/mapstruct-path-explorer.jar|@java_ext_mapstruct|@java_ext_mapstruct|MapStruct completion server
java|all|glob:~/tools/java-extensions/lemminx/lemminx-maven/*.jar|@java_ext_lemminx|@java_ext_lemminx|lemminx + lemminx-maven (pom.xml LSP)
rust|all|rustup,~/.cargo/bin/rustup,/opt/homebrew/opt/rustup/bin/rustup|rustup|rustup|Rust toolchain manager
rust|all|cargo,~/.cargo/bin/cargo|@rust_toolchain|@rust_toolchain|stable toolchain (cargo, rust-analyzer component optional)
rust|all|cargo-nextest,~/.cargo/bin/cargo-nextest|cargo-nextest|cargo-nextest|Rust test runner (JUnit XML)
go|all|dlv,~/go/bin/dlv|delve|delve|Go debug adapter
python|all|python3.14|python@3.14|python|python runner (hardcoded python3.14)
lua|all|lua|lua|lua|lua runner
lua|all|busted,~/.luarocks/bin/busted|@busted|@busted|Lua test runner
js|all|deno|deno|deno|JS/TS runner + debug
cs|all|dotnet|cask:dotnet-sdk|dotnet-sdk|C# run / test
cs|all|netcoredbg,${MASON_BIN}/netcoredbg|@mason_netcoredbg|netcoredbg|C# debug adapter
bash|mac|/opt/homebrew/bin/bash|bash|-|bash runner (bash 5)
bash|all|bashunit,~/.local/bin/bashunit|@bashunit|@bashunit|bash test runner
c|all|gcc|@xcode_clt|gcc|C runner
c|all|g++|@xcode_clt|gcc|C++ runner
ai|all|copilot,~/.local/bin/copilot|@copilot_cli|@copilot_cli|sidekick.nvim CLI (current_ai = copilot)
"

DRY_RUN=0
CHECK_ONLY=0
SELECTED=("${ALL_GROUPS[@]}")
OS=""

# ---------------------------------------------------------------- output helpers

c_green=$'\e[32m'
c_red=$'\e[31m'
c_yellow=$'\e[33m'
c_blue=$'\e[34m'
c_reset=$'\e[0m'

info() { printf '%s==>%s %s\n' "$c_blue" "$c_reset" "$*"; }
warn() { printf '%s!!%s  %s\n' "$c_yellow" "$c_reset" "$*" >&2; }
die() {
    printf '%sxx%s  %s\n' "$c_red" "$c_reset" "$*" >&2
    exit 1
}

run() {
    if ((DRY_RUN)); then
        printf '    [dry-run] %s\n' "$*"
        return 0
    fi
    "$@"
}

usage() { sed -n '2,/^$/p' "$0" | sed 's/^# \{0,1\}//'; }

# ---------------------------------------------------------------- arguments

split_csv() { tr ',' ' ' <<<"$1"; }

parse_args() {
    local only="" skip=""
    while (($#)); do
        case "$1" in
            --only)
                only="${2:?--only needs a value}"
                shift
                ;;
            --only=*) only="${1#*=}" ;;
            --skip)
                skip="${2:?--skip needs a value}"
                shift
                ;;
            --skip=*) skip="${1#*=}" ;;
            --check)
                CHECK_ONLY=1
                # optional group list: `--check core,tools`
                if [[ -n "${2:-}" && "$2" != -* ]]; then
                    only="$2"
                    shift
                fi
                ;;
            --check=*)
                CHECK_ONLY=1
                only="${1#*=}"
                ;;
            --dry-run) DRY_RUN=1 ;;
            --groups)
                for g in "${ALL_GROUPS[@]}"; do printf '  %-7s %s\n' "$g" "${GROUP_DESC[$g]}"; done
                exit 0
                ;;
            -h | --help)
                usage
                exit 0
                ;;
            *) die "unknown option: $1 (see --help)" ;;
        esac
        shift
    done

    local g
    for g in $(split_csv "$only $skip"); do
        [[ -n "${GROUP_DESC[$g]:-}" ]] || die "unknown group: $g (see --groups)"
    done
    if [[ -n "$only" ]]; then
        read -r -a SELECTED <<<"$(split_csv "$only")"
    fi
    if [[ -n "$skip" ]]; then
        local keep=() s
        for g in "${SELECTED[@]}"; do
            s=0
            for x in $(split_csv "$skip"); do [[ "$g" == "$x" ]] && s=1; done
            ((s)) || keep+=("$g")
        done
        SELECTED=("${keep[@]}")
    fi
}

is_selected() {
    local g
    for g in "${SELECTED[@]}"; do [[ "$g" == "$1" ]] && return 0; done
    return 1
}

# ---------------------------------------------------------------- detection

detect_os() {
    case "$(uname -s)" in
        Darwin)
            OS=mac
            # shellcheck disable=SC2016 # the install command is printed literally for the user
            command -v brew >/dev/null 2>&1 \
                || die 'Homebrew is not installed: /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"'
            ;;
        Linux)
            command -v pacman >/dev/null 2>&1 || die "only Arch Linux based systems (pacman) are supported on Linux"
            OS=arch
            ;;
        *) die "unsupported system: $(uname -s)" ;;
    esac
}

has_nerd_font() {
    if [[ "$OS" == mac ]]; then
        # no `| grep -q`: grep exits early, find gets SIGPIPE and pipefail turns it into a failure
        [[ -n "$(find "$HOME/Library/Fonts" /Library/Fonts -maxdepth 1 -iname '*nerd*' -print -quit 2>/dev/null)" ]]
    else
        command -v fc-list >/dev/null 2>&1 && [[ "$(fc-list : family)" == *[Nn]erd\ [Ff]ont* ]]
    fi
}

# Return 0 when any of the comma separated alternatives is present.
is_present() {
    local alt
    local -a alts
    IFS=',' read -r -a alts <<<"$1"
    for alt in "${alts[@]}"; do
        alt="${alt/#\~/$HOME}"
        case "$alt" in
            font:nerd) has_nerd_font && return 0 ;;
            glob:*) compgen -G "${alt#glob:}" >/dev/null && return 0 ;;
            /*) [[ -e "$alt" ]] && return 0 ;;
            *) command -v "$alt" >/dev/null 2>&1 && return 0 ;;
        esac
    done
    return 1
}

# ---------------------------------------------------------------- custom installers

load_sdkman() {
    # sdkman-init.sh references unset variables
    set +u
    # shellcheck source=/dev/null
    source "$SDKMAN_DIR/bin/sdkman-init.sh"
    # read by the `sdk` shell function sourced above: no "set as default?" prompts
    # shellcheck disable=SC2034
    sdkman_auto_answer=true
    set -u
}

ensure_sdkman() {
    if [[ ! -s "$SDKMAN_DIR/bin/sdkman-init.sh" ]]; then
        info "installing SDKMAN (shell rc files are not modified)"
        if ((DRY_RUN)); then
            printf '    [dry-run] curl -s "https://get.sdkman.io?rcupdate=false" | bash\n'
            return 0
        fi
        curl -s "https://get.sdkman.io?rcupdate=false" | bash
    fi
    ((DRY_RUN)) || load_sdkman
}

sdk_java() {
    ensure_sdkman
    # shellcheck disable=SC2086 # JAVA_VERSION is optional: empty -> SDKMAN default candidate
    run sdk install java ${JAVA_VERSION:-}
}

sdk_maven() {
    ensure_sdkman
    run sdk install maven
}

# Java extension installers live in ~/dotfiles/scripts/java and need java on PATH (mvnw / gradlew).
run_java_script() {
    local script="$DOTFILES/scripts/java/$1"
    local sdk_java_home="$SDKMAN_DIR/candidates/java/current"
    if [[ ! -f "$script" ]]; then
        warn "missing installer: $script"
        return 1
    fi
    if ((DRY_RUN)); then
        printf '    [dry-run] bash %s\n' "$script"
        return 0
    fi
    if [[ ! -x "$sdk_java_home/bin/java" ]] && ! command -v java >/dev/null 2>&1; then
        warn "no JDK found, skipping $1 (install the java group first)"
        return 0
    fi
    mkdir -p "$JAVA_EXT_DIR"
    (
        if [[ -x "$sdk_java_home/bin/java" ]]; then
            export JAVA_HOME="$sdk_java_home"
            PATH="$sdk_java_home/bin:$SDKMAN_DIR/candidates/maven/current/bin:$PATH"
        fi
        bash "$script"
    )
}

java_ext_junit() { run_java_script install_junit_console_launcher.sh; }
java_ext_jmockit() { run_java_script install_jmockit_agent.sh; }
java_ext_fernflower() { run_java_script install_fernflower_decompiler.sh; }
java_ext_mapstruct() { run_java_script install_mapstruct_path_explorer_server.sh; }
java_ext_lemminx() { run_java_script lemminx-maven-ls/load_and_build_all.sh; }

rust_toolchain() {
    local rustup
    rustup="$(command -v rustup || true)"
    [[ -z "$rustup" && -x "$HOME/.cargo/bin/rustup" ]] && rustup="$HOME/.cargo/bin/rustup"
    [[ -z "$rustup" && "$OS" == mac ]] && rustup="$(brew --prefix rustup 2>/dev/null)/bin/rustup"
    if [[ -z "$rustup" || ! -x "$rustup" ]] && ! ((DRY_RUN)); then
        warn "rustup not found, skipping the stable toolchain"
        return 0
    fi
    run "${rustup:-rustup}" default stable
}

busted() {
    if ! command -v luarocks >/dev/null 2>&1 && ! ((DRY_RUN)); then
        warn "luarocks not found, skipping busted"
        return 0
    fi
    run luarocks install --local busted
}

bashunit() {
    mkdir -p "$HOME/.local/bin"
    if ((DRY_RUN)); then
        printf '    [dry-run] curl -s https://bashunit.typeddevs.com/install.sh | bash -s %s\n' "$HOME/.local/bin"
        return 0
    fi
    curl -s https://bashunit.typeddevs.com/install.sh | bash -s "$HOME/.local/bin"
}

copilot_cli() {
    if ! command -v npm >/dev/null 2>&1 && ! ((DRY_RUN)); then
        warn "npm not found, skipping the Copilot CLI"
        return 0
    fi
    local prefix
    prefix="$(npm prefix -g 2>/dev/null || true)"
    if [[ -n "$prefix" && -w "$prefix/lib" ]]; then
        run npm install -g @github/copilot
    else
        # system npm (pacman) needs root for -g; keep it in the user's ~/.local instead
        run npm install -g --prefix "$HOME/.local" @github/copilot
    fi
}

mason_netcoredbg() {
    warn "netcoredbg has no Homebrew formula: run :MasonInstall netcoredbg inside Neovim"
}

xcode_clt() {
    if xcode-select -p >/dev/null 2>&1; then
        return 0
    fi
    info "installing Xcode Command Line Tools (a system dialog opens; re-run this script afterwards)"
    run xcode-select --install || true
}

# ---------------------------------------------------------------- package managers

brew_install() {
    local -a formulas=() casks=()
    local p
    for p in "$@"; do
        if [[ "$p" == cask:* ]]; then casks+=("${p#cask:}"); else formulas+=("$p"); fi
    done
    ((${#formulas[@]} == 0)) || run brew install "${formulas[@]}"
    ((${#casks[@]} == 0)) || run brew install --cask "${casks[@]}"
}

aur_helper() {
    local h
    for h in yay paru; do
        command -v "$h" >/dev/null 2>&1 && {
            echo "$h"
            return 0
        }
    done
    return 1
}

pacman_install() {
    local -a repo=() aur=()
    local p helper
    for p in "$@"; do
        if pacman -Si "$p" >/dev/null 2>&1 || pacman -Sg "$p" >/dev/null 2>&1; then
            repo+=("$p")
        else
            aur+=("$p")
        fi
    done
    ((${#repo[@]} == 0)) || run sudo pacman -S --needed --noconfirm "${repo[@]}"
    if ((${#aur[@]})); then
        if helper="$(aur_helper)"; then
            run "$helper" -S --needed --noconfirm "${aur[@]}"
        else
            warn "no AUR helper (yay/paru) found, install manually: ${aur[*]}"
        fi
    fi
}

# ---------------------------------------------------------------- main flow

# Short display name of a check: first alternative, file name for paths, directory for globs.
label() {
    local l="${1%%,*}"
    l="${l#glob:}"
    [[ "$l" == */*\** ]] && l="${l%/*}"
    [[ "$l" == */* ]] && l="${l##*/}"
    printf '%s' "$l"
}

# Print one status line per selected dependency; fill MISSING_PKGS / MISSING_FUNCS.
MISSING_PKGS=()
MISSING_FUNCS=()
MISSING_COUNT=0

scan() {
    local collect="$1" group os check brew pac desc target
    MISSING_PKGS=()
    MISSING_FUNCS=()
    MISSING_COUNT=0
    while IFS='|' read -r group os check brew pac desc; do
        [[ -z "$group" ]] && continue
        is_selected "$group" || continue
        [[ "$os" == all || "$os" == "$OS" ]] || continue
        target="$brew"
        [[ "$OS" == arch ]] && target="$pac"
        if is_present "$check"; then
            printf '  %s✔%s %-6s %-30s %s\n' "$c_green" "$c_reset" "$group" "$(label "$check")" "$desc"
            continue
        fi
        MISSING_COUNT=$((MISSING_COUNT + 1))
        printf '  %s✘%s %-6s %-30s %s\n' "$c_red" "$c_reset" "$group" "$(label "$check")" "$desc"
        ((collect)) || continue
        case "$target" in
            -) warn "${check%%,*}: no package on this system, install it manually" ;;
            @*) [[ " ${MISSING_FUNCS[*]} " == *" ${target#@} "* ]] || MISSING_FUNCS+=("${target#@}") ;;
            *) [[ " ${MISSING_PKGS[*]} " == *" $target "* ]] || MISSING_PKGS+=("$target") ;;
        esac
    done <<<"$DEPS"
}

version_ge() { [[ "$(printf '%s\n%s\n' "$2" "$1" | sort -V | head -n1)" == "$2" ]]; }

check_nvim_version() {
    command -v nvim >/dev/null 2>&1 || return 0
    local v
    v="$(nvim --version)"
    v="${v%%$'\n'*}"
    v="${v#NVIM v}"
    v="${v%%-*}"
    version_ge "$v" "$MIN_NVIM_VERSION" || warn "Neovim $v is older than the required $MIN_NVIM_VERSION"
}

main() {
    parse_args "$@"
    detect_os
    info "system: $OS, groups: ${SELECTED[*]}"

    if ((CHECK_ONLY)); then
        scan 0
        check_nvim_version
        if ((MISSING_COUNT == 0)); then
            info "all dependencies are installed"
        else
            info "$MISSING_COUNT missing, run without --check to install them"
        fi
        return 0
    fi

    scan 1
    if ((${#MISSING_PKGS[@]} == 0 && ${#MISSING_FUNCS[@]} == 0)); then
        check_nvim_version
        info "nothing to install"
        return 0
    fi

    if ((${#MISSING_PKGS[@]})); then
        info "installing packages: ${MISSING_PKGS[*]}"
        if [[ "$OS" == mac ]]; then brew_install "${MISSING_PKGS[@]}"; else pacman_install "${MISSING_PKGS[@]}"; fi
    fi

    # Custom installers run after packages: they need curl/zip/git/npm/luarocks from above,
    # and the Java extensions need the JDK installed by sdk_java (listed before them).
    local f
    for f in "${MISSING_FUNCS[@]}"; do
        info "running $f"
        "$f" || warn "$f failed"
    done

    if ! ((DRY_RUN)); then
        info "result:"
        scan 0
    fi
    check_nvim_version

    cat <<'EOF'

Next steps:
  - start nvim: lazy.nvim installs plugins, Mason installs LSPs / DAP adapters / formatters / linters
  - ~/.local/bin, ~/.cargo/bin, ~/.luarocks/bin and ~/go/bin must be on PATH
  - per-project test tools are not installed globally: pytest (project venv), jest (npm i -D jest)
  - see DEPENDENCIES.md for the manual items (kulala-core build, VS Code bundles, ...)
EOF
}

main "$@"
