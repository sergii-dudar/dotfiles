#!/usr/bin/env bash
# Shared engine of the interactive theme pickers in this directory (alacritty.sh, ghostty.sh, kitty.sh, wezterm.sh,
# foot.sh): the theme list goes into fzf, every cursor move applies the highlighted theme to the terminal this shell is
# drawn in, Enter keeps it (written to that terminal's config), Esc puts the previous one back. See README.md.
#
# A terminal script defines tt_name, tt_config and the functions tt_list, tt_current, tt_preview NAME, tt_apply NAME,
# tt_restore, then calls `tt_main "$@"`. fzf re-invokes the script with `preview NAME` / `swatch NAME` for the
# highlighted entry. Two ways to apply a theme live:
#   - config rewrite: alacritty and wezterm reload a changed config on their own, kitty on SIGUSR1 -- so the preview
#     IS the final result, with every override the config puts on top of a theme.
#   - escape codes: ghostty and foot cannot reload from outside, so the preview sends OSC 4/10/11/12/17/19 (through
#     tmux's passthrough when inside tmux) and Enter writes the config; Esc sends the OSC resets.
#
# A first argument `dark` (or `night`) / `light` (or `day`) narrows the list to themes with a dark / light background:
# the terminal script then also defines tt_bgs, which prints "NAME<tab>background" for every theme (empty background
# when the theme sets none, counted as dark like every terminal's default), and tt_themes filters on its brightness.
#
# Knobs, mainly for testing: TT_TERMINAL=<name> skips the detection, TT_CONFIG=<file> points a script at another config
# (its themes directory is derived from the config's directory), TT_TTY=<file> receives the escape codes.

# shellcheck disable=SC2154 # tt_name and tt_config are set by the terminal script that sources this file
set -u

# A shell started by a GUI terminal on macOS may lack Homebrew on PATH (fzf, tmux). No-op when already there.
for d in /opt/homebrew/bin /usr/local/bin; do
    [ -d "$d" ] && case ":$PATH:" in *":$d:"*) ;; *) PATH="$d:$PATH" ;; esac
done
export PATH

tt_die() {
    printf '%s\n' "$*" >&2
    exit 1
}

# Which terminal draws this shell. Inside tmux only the client's XTVERSION reply (#{client_termtype}) is reliable: the
# GHOSTTY_* / KITTY_* / WEZTERM_* variables leak from the client that started the server into every pane. Alacritty
# does not answer XTVERSION, so an empty reply with TERM alacritty or xterm-256color means alacritty.
tt_detect() {
    if [ -n "${TT_TERMINAL:-}" ]; then
        printf '%s\n' "$TT_TERMINAL"
        return
    fi
    local type name
    if [ -n "${TMUX:-}" ]; then
        type="$(tmux display -p '#{client_termtype}' 2>/dev/null || true)"
        name="$(tmux display -p '#{client_termname}' 2>/dev/null || true)"
        case "$type" in
            ghostty*) echo ghostty ;;
            kitty*) echo kitty ;;
            WezTerm*) echo wezterm ;;
            foot*) echo foot ;;
            "")
                case "$name" in
                    alacritty* | xterm-256color) echo alacritty ;;
                    *) echo unknown ;;
                esac
                ;;
            *) echo unknown ;;
        esac
        return
    fi
    case "${TERM:-}" in
        xterm-ghostty) echo ghostty && return ;;
        xterm-kitty) echo kitty && return ;;
        foot*) echo foot && return ;;
        alacritty*) echo alacritty && return ;;
    esac
    case "${TERM_PROGRAM:-}" in
        ghostty) echo ghostty && return ;;
        WezTerm) echo wezterm && return ;;
    esac
    if [ -n "${ALACRITTY_WINDOW_ID:-}" ]; then
        echo alacritty
    elif [ -n "${WEZTERM_PANE:-}" ]; then
        echo wezterm
    elif [ -n "${KITTY_PID:-}" ]; then
        echo kitty
    else
        echo unknown
    fi
}

tt_require() {
    local here
    here="$(tt_detect)"
    [ "$here" = "$1" ] \
        || tt_die "$(basename "$0"): this shell is drawn by '$here', not $1 -- run $here.sh here (TT_TERMINAL=$1 forces)"
}

# ---------------------------------------------------------------- escape codes (ghostty, foot) ----------------------
tt_tty="${TT_TTY:-/dev/tty}"

# Writes a raw escape sequence to the terminal. Inside tmux it goes through the DCS passthrough (`allow-passthrough on`
# in .tmux.conf; every ESC inside is doubled) so the outer terminal gets it instead of tmux.
tt_emit() {
    local seq="$1"
    if [ -n "${TMUX:-}" ]; then
        seq="${seq//$'\e'/$'\e\e'}"
        # shellcheck disable=SC1003 # \\ is printf's backslash, the DCS terminator ESC \
        printf '\033Ptmux;%s\033\\' "$seq" >"$tt_tty"
    else
        printf '%s' "$seq" >"$tt_tty"
    fi
}

# "#rrggbb" or "rrggbb" -> "rgb:rr/gg/bb", the X11 form every terminal accepts in OSC 4/10/11/12/17/19.
tt_rgb() {
    local h="${1#\#}"
    printf 'rgb:%s/%s/%s' "${h:0:2}" "${h:2:2}" "${h:4:2}"
}

tt_is_hex() { [[ "$1" =~ ^#?[0-9a-fA-F]{6}$ ]]; }

# The colours a parser collects before tt_osc_apply: palette 0-15, fg, bg, cursor, selection (empty = leave as is).
declare -a tt_c=()
tt_fg='' tt_bg='' tt_cursor='' tt_sel_bg='' tt_sel_fg=''

tt_colors_reset() {
    tt_c=()
    tt_fg='' tt_bg='' tt_cursor='' tt_sel_bg='' tt_sel_fg=''
}

# Key/value from a parser: "$1" key name as the terminal spells it, "$2" the value. Shared by ghostty.sh and foot.sh.
tt_colors_set() {
    local key="$1" val="$2" idx
    case "$key" in
        palette) # ghostty: palette = N=#hex
            idx="${val%%=*}"
            val="${val#*=}"
            [[ "$idx" =~ ^[0-9]+$ ]] && [ "$idx" -le 15 ] && tt_is_hex "$val" && tt_c[idx]="$val"
            ;;
        regular[0-7]) tt_is_hex "$val" && tt_c[${key#regular}]="$val" ;;
        bright[0-7]) tt_is_hex "$val" && tt_c[${key#bright} + 8]="$val" ;;
        foreground) tt_is_hex "$val" && tt_fg="$val" ;;
        background) tt_is_hex "$val" && tt_bg="$val" ;;
        cursor-color) tt_is_hex "$val" && tt_cursor="$val" ;;
        cursor) # foot: cursor=<text> <cursor>
            val="${val##* }"
            tt_is_hex "$val" && tt_cursor="$val"
            ;;
        selection-background) tt_is_hex "$val" && tt_sel_bg="$val" ;;
        selection-foreground) tt_is_hex "$val" && tt_sel_fg="$val" ;;
    esac
}

# OSC 4 palette, 10 foreground, 11 background, 12 cursor, 17/19 selection (terminals without 17/19 ignore them).
tt_osc_apply() {
    local i seq=''
    for i in "${!tt_c[@]}"; do
        seq+=$'\e]4;'"$i;$(tt_rgb "${tt_c[$i]}")"$'\e\\'
    done
    [ -n "$tt_fg" ] && seq+=$'\e]10;'"$(tt_rgb "$tt_fg")"$'\e\\'
    [ -n "$tt_bg" ] && seq+=$'\e]11;'"$(tt_rgb "$tt_bg")"$'\e\\'
    [ -n "$tt_cursor" ] && seq+=$'\e]12;'"$(tt_rgb "$tt_cursor")"$'\e\\'
    [ -n "$tt_sel_bg" ] && seq+=$'\e]17;'"$(tt_rgb "$tt_sel_bg")"$'\e\\'
    [ -n "$tt_sel_fg" ] && seq+=$'\e]19;'"$(tt_rgb "$tt_sel_fg")"$'\e\\'
    [ -n "$seq" ] && tt_emit "$seq"
}

# Back to the colours of the terminal's own config: OSC 104 (whole palette), 110, 111, 112, 117, 119.
tt_osc_reset() {
    tt_emit $'\e]104\e\\\e]110\e\\\e]111\e\\\e]112\e\\\e]117\e\\\e]119\e\\'
}

# ---------------------------------------------------------------- config rewrite ------------------------------------
# $1 file, $2 perl expression over the whole file (-0). Writes back into the same inode (alacritty/wezterm watch the
# file) and keeps every other byte, including a missing final newline. The replacement text travels in $TT_NEW so
# theme names need no escaping.
tt_rewrite() {
    local file="$1" expr="$2" out
    out="$(perl -0 -pe "$expr" <"$file" && printf x)" || tt_die "could not rewrite $file"
    out="${out%x}"
    printf '%s' "$out" >"$file"
}

# Number of lines of $1 matching the extended regex $2.
tt_count() {
    grep -c -E -- "$2" "$1" 2>/dev/null || true
}

# ---------------------------------------------------------------- dark / light ------------------------------------
tt_mode='' # dark, light or empty (every theme), from the first argument

# "NAME<tab>background" of theme files: the last six hex digits of the first `background` key of each file
# (`background #hex`, `background = '#hex'`, `background=hex`), an empty value when there is none. TT_SECTIONS=REGEX
# only takes it inside a matching [section] (alacritty's colors.primary, foot's colors), TT_EXT=.EXT is stripped from
# the name, TT_ZIP=1 reads `unzip -c` output from stdin, where an "  inflating: PATH  " line (" extracting: " for a
# stored file) starts the next file. Hex runs are spelled out: mawk has no {6}.
tt_bg_scan() {
    awk -v sections="${TT_SECTIONS:-}" -v ext="${TT_EXT:-}" -v zip="${TT_ZIP:-}" '
        BEGIN { x = "[0-9a-fA-F]"; hex6 = x x x x x x }
        function start(path, name) {
            name = path
            sub(/.*\//, "", name)
            if (ext != "" && substr(name, length(name) - length(ext) + 1) == ext)
                name = substr(name, 1, length(name) - length(ext))
            names[++n] = name
            section = ""
        }
        zip && /^ *(inflating|extracting): / {
            p = $0
            sub(/^ *[a-z]+: /, "", p)
            sub(/[ \t]+$/, "", p)
            start(p)
            next
        }
        !zip && FNR == 1 { start(FILENAME) }
        n == 0 { next }
        /^[ \t]*\[.*\][ \t]*$/ { section = $0; gsub(/^[ \t]*\[|\][ \t]*$/, "", section); next }
        /^[ \t]*background([ \t]|=)/ {
            if (bg[n] != "" || (sections != "" && section !~ sections)) next
            v = $0
            sub(/#[^0-9a-fA-F].*$/, "", v) # a trailing comment, not the colour
            if (match(v, hex6 "[^0-9a-fA-F]*$")) bg[n] = substr(v, RSTART, 6)
        }
        END { for (i = 1; i <= n; i++) printf "%s\t%s\n", names[i], bg[i] }
    ' "$@"
}

# tt_bgs lines -> the names of the tt_mode themes, sorted; the first line of a name wins (user themes come first).
# Dark = perceived brightness (0.299 R + 0.587 G + 0.114 B) of the background below half; no background = dark.
tt_bg_filter() {
    awk -F '\t' -v mode="$tt_mode" '
        BEGIN { x = "[0-9a-fA-F]"; hex6 = "^" x x x x x x "$" }
        function nibble(h, i) { return index("0123456789abcdef", substr(h, i, 1)) - 1 }
        function byte(h, i) { return nibble(h, i) * 16 + nibble(h, i + 1) }
        seen[$1]++ { next }
        {
            dark = 1
            if ($2 ~ hex6) {
                h = tolower($2)
                dark = 0.299 * byte(h, 1) + 0.587 * byte(h, 3) + 0.114 * byte(h, 5) < 127.5
            }
            if (dark == (mode == "dark")) print $1
        }
    ' | sort
}

tt_themes() {
    if [ -z "$tt_mode" ]; then
        tt_list
    else
        tt_bgs | tt_bg_filter
    fi
}

# ---------------------------------------------------------------- UI --------------------------------------------------
# Preview pane: the palette as blocks plus a text sample. SGR 30-37/48;5;N use the terminal's own palette, which the
# highlighted theme has just replaced, so the blocks show the real colours.
tt_swatch() {
    local i
    printf '%s\n\n' "$1"
    for i in 0 1 2 3 4 5 6 7; do printf '\033[48;5;%dm    \033[0m ' "$i"; done
    printf '\n'
    for i in 8 9 10 11 12 13 14 15; do printf '\033[48;5;%dm    \033[0m ' "$i"; done
    printf '\n\n'
    printf 'text  \033[1mbold\033[0m  \033[3mitalic\033[0m  \033[4munderline\033[0m  '
    for i in 1 2 3 4 5 6; do printf '\033[3%dmcolour%d\033[0m ' "$i" "$i"; done
    printf '\n'
}

tt_pick() {
    local self="$1" list current idx choice orig
    command -v fzf >/dev/null 2>&1 || tt_die "fzf is required"
    tt_require "$tt_name"
    list="$(tt_themes)"
    [ -n "$list" ] || tt_die "$tt_name: no ${tt_mode:+$tt_mode }themes found"
    current="$(tt_current)"
    idx="$(printf '%s\n' "$list" | grep -n -x -F -- "$current" | head -1 | cut -d: -f1)"
    : "${idx:=1}"
    # Esc restores the config byte for byte, whatever the previews did to it.
    orig="$(mktemp "${TMPDIR:-/tmp}/terminal-theme.XXXXXX")" || tt_die "mktemp failed"
    # shellcheck disable=SC2064 # $orig is final here, expand it now
    trap "rm -f '$orig'" EXIT
    cat "$tt_config" >"$orig"
    choice="$(printf '%s\n' "$list" | fzf --sync --ansi --cycle --no-multi --layout=reverse \
        --prompt="$tt_name ${tt_mode:+$tt_mode }theme > " \
        --header="now: ${current:-?}   move = preview   Enter = keep   Esc = back" \
        --bind "start:pos($idx)" \
        --bind "focus:execute-silent($self preview {})" \
        --preview "$self swatch {}" --preview-window=down,8,wrap)" || true
    if [ -n "$choice" ]; then
        tt_apply "$choice"
        printf '%s theme: %s\n' "$tt_name" "$choice"
    else
        cat "$orig" >"$tt_config"
        tt_restore
        printf '%s theme kept: %s\n' "$tt_name" "${current:-the previous one}"
    fi
}

tt_main() {
    local self
    self="$(cd "$(dirname "$0")" && pwd)/$(basename "$0")"
    case "${1:-}" in
        dark | night) tt_mode=dark && shift ;;
        light | day) tt_mode=light && shift ;;
    esac
    case "${1:-}" in
        list) tt_themes ;;
        current) tt_current ;;
        preview) tt_preview "${2:?theme name}" ;;
        apply) tt_apply "${2:?theme name}" ;;
        restore) tt_restore ;;
        swatch) tt_swatch "${2:?theme name}" ;;
        "") tt_pick "$self" ;;
        *) tt_die "usage: $(basename "$0") [dark | light] [list] | current | preview NAME | apply NAME | restore |" \
            "swatch NAME" ;;
    esac
}