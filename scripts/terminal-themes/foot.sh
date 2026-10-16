#!/usr/bin/env bash
# Interactive theme picker for foot: the themes are the files in ~/.config/foot/themes (this repo) and the ones the foot
# package ships in /usr/share/foot/themes. foot cannot be told to reload its config, so moving in the list sends the
# theme's colours as escape codes (OSC 4/10/11/12/17/19, through tmux's passthrough when inside tmux) with foot.ini's
# own [colors] overrides (cursor, selection) on top, which is what the theme will look like in a new window. Enter
# writes the `include=` line of foot.ini, Esc sends the OSC resets so the window shows foot.ini's colours again.
# Written on macOS without a foot to try it on: unverified.
#
#   foot.sh                 pick interactively
#   foot.sh dark|light      only themes with a dark / light background (also `foot.sh dark list`)
#   foot.sh list|current    the names / the active theme
#   foot.sh preview NAME    only send the colours (this window, until foot restarts)
#   foot.sh apply NAME      write the include line without the picker
#   foot.sh restore         send the OSC resets

# shellcheck source=common.sh
. "$(dirname "$0")/common.sh"

tt_name=foot
tt_config="${TT_CONFIG:-$HOME/.config/foot/foot.ini}"

tt_dirs() {
    local d
    for d in "$(dirname "$tt_config")/themes" /usr/share/foot/themes; do
        [ -d "$d" ] && printf '%s\n' "$d"
    done
    return 0
}

tt_path() {
    local d
    while IFS= read -r d; do
        [ -f "$d/$1" ] && printf '%s\n' "$d/$1" && return 0
    done < <(tt_dirs)
    return 1
}

tt_list() {
    local d
    while IFS= read -r d; do
        find -H "$d" -mindepth 1 -maxdepth 1 -type f | sed 's|.*/||'
    done < <(tt_dirs) | sort -u
}

tt_bgs() { # user themes first, so they win over a shipped theme of the same name
    local d f
    local -a files=()
    while IFS= read -r d; do
        while IFS= read -r f; do files+=("$f"); done < <(find -H "$d" -mindepth 1 -maxdepth 1 -type f)
    done < <(tt_dirs)
    [ ${#files[@]} -eq 0 ] || TT_SECTIONS='^colors(-dark)?$' tt_bg_scan "${files[@]}"
}

tt_current() {
    grep -E '^include=' "$tt_config" | head -1 | sed 's|^include=||; s|.*/||'
}

# key=value lines of the [colors] / [colors-dark] sections of a foot ini into tt_c & co (hex without #, inline comments)
tt_read() {
    local line key val section=''
    while IFS= read -r line || [ -n "$line" ]; do
        case "$line" in
            \#* | "") continue ;;
            \[*\]) section="${line#\[}" && section="${section%%\]*}" && continue ;;
        esac
        case "$section" in colors | colors-dark) ;; *) continue ;; esac
        case "$line" in *=*) ;; *) continue ;; esac
        key="${line%%=*}"
        key="${key//[[:space:]]/}"
        val="${line#*=}"
        val="${val%%#*}"
        val="${val#"${val%%[![:space:]]*}"}"
        val="${val%"${val##*[![:space:]]}"}"
        tt_colors_set "$key" "$val"
    done <"$1"
}

tt_preview() {
    local file
    file="$(tt_path "$1")" || tt_die "no such theme: $1"
    tt_colors_reset
    tt_read "$file"
    tt_read "$tt_config" # foot.ini's own keys come after the include, so they win, as in foot itself
    tt_osc_apply
}

tt_apply() {
    local file
    file="$(tt_path "$1")" || tt_die "no such theme: $1"
    case "$(tt_count "$tt_config" '^include=')" in
        1) ;;
        0) tt_die "no active 'include=' line in $tt_config" ;;
        *) tt_die "more than one active 'include=' line in $tt_config" ;;
    esac
    case "$file" in "$HOME"/*) file="~${file#"$HOME"}" ;; esac # keep the ~ form foot.ini uses
    # shellcheck disable=SC2016 # $ENV{TT_NEW} is for perl
    TT_NEW="$file" tt_rewrite "$tt_config" 's{^include=.*$}{include=$ENV{TT_NEW}}m'
    tt_preview "$1"
    printf 'written to %s; new foot windows use it\n' "$tt_config" >&2
}

tt_restore() { tt_osc_reset; }

tt_main "$@"