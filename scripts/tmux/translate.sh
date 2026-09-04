#!/usr/bin/env bash
# Translate text without entering tmux copy-mode.
#
#   translate.sh                  interactive prompt in a tmux popup (REPL)
#   translate.sh clipboard        translate the system clipboard
#   translate.sh pipe             translate stdin (copy-mode selection)
#   translate.sh buffer           translate the top tmux paste buffer
#   translate.sh text <words...>  translate the given words
#   translate.sh --print <words>  print translation to stdout, no popup
#
# Direction is auto-detected: Cyrillic input goes to @translate-from, anything
# else to @translate-to. The engine is the vendored translator/translator.py,
# so this has no tmux-plugin dependency.

set -u

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SELF="$SCRIPT_DIR/$(basename "${BASH_SOURCE[0]}")"
TRANSLATOR="${TRANSLATE_ENGINE:-$SCRIPT_DIR/translator/translator.py}"

# macOS has no `python`, only `python3`; Arch has both.
PY="$(command -v python3 || command -v python || true)"

tmux_opt() {
    local value=""
    [ -n "${TMUX:-}" ] && value="$(tmux show-option -gqv "$1" 2>/dev/null)"
    if [ -n "$value" ]; then printf '%s' "$value"; else printf '%s' "$2"; fi
}

LANG_FROM="$(tmux_opt '@translate-from' 'en')"
LANG_TO="$(tmux_opt '@translate-to' 'uk')"
ENGINE="$(tmux_opt '@translate-engine' 'google')"
WIDTH="$(tmux_opt '@translate-width' '70%')"
HEIGHT="$(tmux_opt '@translate-height' '60%')"

die() { printf '%s\n' "$*" >&2; exit 1; }

# Cyrillic anywhere in the text means we are going the other way.
detect_direction() {
    "$PY" -c 'import sys
t = sys.argv[1]
print("back" if any("Ѐ" <= c <= "ӿ" for c in t) else "forth")' "$1"
}

translate() {
    local text="$1" from to engine multi out
    [ -n "${text//[[:space:]]/}" ] || return 0

    if [ "$(detect_direction "$text")" = back ]; then
        from="$LANG_TO"; to="$LANG_FROM"
    else
        from="$LANG_FROM"; to="$LANG_TO"
    fi

    multi=0
    case "$ENGINE" in *'|'*) multi=1 ;; esac

    local old_ifs="$IFS"
    IFS='|'
    # shellcheck disable=SC2086
    set -- $ENGINE
    IFS="$old_ifs"

    for engine in "$@"; do
        [ "$multi" -eq 1 ] && printf -- '--- %s ---\n' "$engine"
        # Pass the selection as a single argv element: no xargs, so quotes,
        # apostrophes and newlines survive intact.
        out="$("$PY" "$TRANSLATOR" --engine="$engine" --from="$from" --to="$to" "$text" 2>&1)"
        if [ -n "$out" ]; then
            printf '%s\n' "$out"
        else
            printf '(no result from %s for %s -> %s)\n' "$engine" "$from" "$to"
        fi
    done
}

clipboard_text() {
    if command -v pbpaste >/dev/null 2>&1; then
        pbpaste
    elif [ -n "${WAYLAND_DISPLAY:-}" ] && command -v wl-paste >/dev/null 2>&1; then
        wl-paste -n
    elif command -v xclip >/dev/null 2>&1; then
        xclip -o -selection clipboard 2>/dev/null
    elif command -v xsel >/dev/null 2>&1; then
        xsel -ob 2>/dev/null
    fi
}

pause() {
    printf '\n[Enter to close]'
    read -r _ || true
}

repl() {
    printf 'translate  %s <-> %s  (empty line or Ctrl-C to close)\n\n' "$LANG_FROM" "$LANG_TO"
    while true; do
        printf '> '
        IFS= read -r line || break
        [ -n "${line//[[:space:]]/}" ] || break
        printf '\n'
        translate "$line"
        printf '\n'
    done
}

# Runs inside the popup; mode and payload arrive through the environment so
# nothing has to survive a second round of shell quoting.
inner() {
    case "${TR_MODE:-prompt}" in
        prompt) repl ;;
        *) translate "${TR_TEXT:-}"; pause ;;
    esac
}

popup() {
    tmux popup -w "$WIDTH" -h "$HEIGHT" -e "TR_MODE=$1" -e "TR_TEXT=${2:-}" -E "'$SELF' --inner"
}

[ -n "$PY" ] || die 'translate.sh: no python3/python on PATH'
[ -f "$TRANSLATOR" ] || die "translate.sh: engine not found at $TRANSLATOR"

case "${1:-prompt}" in
    --inner)
        inner
        ;;
    --print)
        shift
        translate "$*"
        ;;
    prompt)
        popup prompt
        ;;
    clipboard)
        popup clipboard "$(clipboard_text)"
        ;;
    pipe)
        popup pipe "$(cat)"
        ;;
    buffer)
        popup buffer "$(tmux save-buffer - 2>/dev/null)"
        ;;
    text)
        shift
        popup text "$*"
        ;;
    *)
        die "translate.sh: unknown mode '$1' (prompt|clipboard|buffer|pipe|text|--print)"
        ;;
esac