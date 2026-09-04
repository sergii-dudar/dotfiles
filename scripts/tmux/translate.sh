#!/usr/bin/env bash
# Translate text without entering tmux copy-mode.
#
#   translate.sh                  interactive prompt in a tmux popup (REPL)
#   translate.sh clipboard        translate the system clipboard
#   translate.sh primary          translate the PRIMARY selection (X11/Wayland)
#   translate.sh pipe             translate stdin (tmux copy-mode / foot selection)
#   translate.sh buffer           translate the top tmux paste buffer
#   translate.sh text <words...>  translate the given words
#   translate.sh --print <words>  print translation to stdout, no popup
#
# Direction is auto-detected: Cyrillic input goes to @translate-from, anything
# else to @translate-to. The engine is the vendored translator/translator.py,
# so this has no tmux-plugin dependency.
#
# Output always lands in a tmux popup. Terminal keybindings (foot pipe-selected,
# alacritty keyboard.bindings) invoke this from *outside* tmux, so the popup is
# aimed at an attached client explicitly -- see popup() below.

set -u

# foot/alacritty spawn us with the GUI session's PATH, which on macOS omits
# Homebrew -- so tmux and python3 would not be found. Cheap to add, no-op if absent.
for d in /opt/homebrew/bin /usr/local/bin; do
    [ -d "$d" ] && case ":$PATH:" in *":$d:"*) ;; *) PATH="$d:$PATH" ;; esac
done
export PATH

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SELF="$SCRIPT_DIR/$(basename "${BASH_SOURCE[0]}")"
TRANSLATOR="${TRANSLATE_ENGINE:-$SCRIPT_DIR/translator/translator.py}"

# macOS has no `python`, only `python3`; Arch has both.
PY="$(command -v python3 || command -v python || true)"

# Not gated on $TMUX: when foot/alacritty invoke us there is no $TMUX, but the
# server is still running and still holds the user's @translate-* settings.
tmux_opt() {
    local value=""
    value="$(tmux show-option -gqv "$1" 2>/dev/null)" || value=""
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
        # The vendored engine speaks stdlib urllib on both platforms, so this
        # can only fire for a custom $TRANSLATE_ENGINE or an explicit
        # TRANSLATE_HTTP=requests. Note that on macOS neither `pip3 install
        # --user` (PEP 668 on Homebrew python) nor `brew install` (no such
        # formula) gets you `requests` -- a venv is the way.
        case "$out" in
            *"No module named 'requests'"*)
                out="translate.sh: $PY has no 'requests' module.
    python3 -m venv ~/.local/share/translate-venv
    ~/.local/share/translate-venv/bin/pip install requests
then point translate.sh at that interpreter (PY= near the top).
The bundled engine does not need it -- unset TRANSLATE_HTTP instead."
                ;;
        esac
        if [ -n "$out" ]; then
            printf '%s\n' "$out"
        else
            printf '(no result from %s for %s -> %s)\n' "$engine" "$from" "$to"
        fi
    done
}

# Mouse-selected text in a browser or any other app lands in PRIMARY on
# X11/Wayland. macOS has no PRIMARY, so fall back to the clipboard there.
primary_text() {
    if [ -n "${WAYLAND_DISPLAY:-}" ] && command -v wl-paste >/dev/null 2>&1; then
        wl-paste --primary -n
    elif command -v xclip >/dev/null 2>&1; then
        xclip -o -selection primary 2>/dev/null
    elif command -v xsel >/dev/null 2>&1; then
        xsel -op 2>/dev/null
    else
        clipboard_text
    fi
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
    printf '\n[Enter, Esc or q to close]'
    while IFS= read -rsn1 key; do
        # empty = Enter (read -n1 strips the newline); $'\e' = Esc
        case "$key" in
            '' | $'\e' | q | Q) break ;;
        esac
    done
}

# Reads one line into $LINE, but bails out if the line is opened with Esc.
# Only the first character is read raw (a lone Esc is not delivered in canonical
# mode); it is then handed to readline via -i rather than echoed by hand, so
# backspace can erase it like any other character.
read_line() {
    local first rest
    LINE=""
    IFS= read -rsn1 -p '> ' first || return 1
    [ "$first" = $'\e' ] && return 1
    [ -z "$first" ] && return 0
    if [ "${BASH_VERSINFO[0]}" -ge 4 ]; then
        IFS= read -re -i "$first" LINE || return 1
    else
        # stock macOS bash is 3.2, which has no `read -i`. Echo the character
        # ourselves; the cost is that backspace cannot reach back over it.
        printf '%s' "$first"
        IFS= read -r rest || return 1
        LINE="$first$rest"
    fi
}

repl() {
    printf 'translate  %s <-> %s  (Esc, q, empty line or Ctrl-C to close)\n\n' "$LANG_FROM" "$LANG_TO"
    while true; do
        read_line || break
        [ -n "${LINE//[[:space:]]/}" ] || break
        # a lone q quits; to translate the letter itself, type it with anything else
        case "$LINE" in q | Q) break ;; esac
        printf '\n'
        translate "$LINE"
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

# Without $TMUX tmux has no "current client" to draw on. With exactly one client
# attached it guesses right, but with several it can pick the wrong window, so
# name the most recently active one explicitly.
newest_client() {
    tmux list-clients -F '#{client_activity} #{client_name}' 2>/dev/null \
        | sort -rn | head -1 | cut -d' ' -f2-
}

# The two branches are spelled out rather than built in an array: expanding an
# empty array trips `set -u` on bash 3.2, which is what macOS still ships.
popup() {
    local mode="$1" text="${2:-}" client=""
    if [ -z "${TMUX:-}" ]; then
        client="$(newest_client)"
        [ -n "$client" ] || die 'translate.sh: no attached tmux client to draw the popup on'
    fi
    if [ -n "$client" ]; then
        tmux popup -c "$client" -w "$WIDTH" -h "$HEIGHT" \
            -e "TR_MODE=$mode" -e "TR_TEXT=$text" -E "'$SELF' --inner"
    else
        tmux popup -w "$WIDTH" -h "$HEIGHT" \
            -e "TR_MODE=$mode" -e "TR_TEXT=$text" -E "'$SELF' --inner"
    fi
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
    primary)
        popup primary "$(primary_text)"
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
        die "translate.sh: unknown mode '$1' (prompt|clipboard|primary|buffer|pipe|text|--print)"
        ;;
esac