#!/usr/bin/env bash
# Interactive theme picker for ghostty: the themes are the ones `ghostty +list-themes` knows, i.e. the files shipped in
# the app's resources plus your own in ~/.config/ghostty/themes. Ghostty cannot be told to reload its config from
# outside, so moving in the list sends the theme's colours as escape codes (OSC 4/10/11/12/17/19, through tmux's
# passthrough when inside tmux) with the config's own overrides (background, cursor, selection) on top, which is what
# the theme will look like after a reload. Enter writes `theme = NAME` to the config, Esc sends the OSC resets so the
# window shows the config's colours again. After Enter the other windows pick the theme up on reload_config
# (cmd+shift+, on macOS, ctrl+shift+, on Linux); this window already shows it.
#
#   ghostty.sh                 pick interactively
#   ghostty.sh list|current    the names / the active theme
#   ghostty.sh preview NAME    only send the colours (until the next reload)
#   ghostty.sh apply NAME      write the theme line without the picker
#   ghostty.sh restore         send the OSC resets

# shellcheck source=common.sh
. "$(dirname "$0")/common.sh"

tt_name=ghostty
tt_config="${TT_CONFIG:-$HOME/.config/ghostty/config}"

# user themes first, then the ones shipped with ghostty (macOS app bundle, Linux package)
tt_dirs() {
    local d
    for d in "$(dirname "$tt_config")/themes" "${GHOSTTY_RESOURCES_DIR:-/nonexistent}/themes" \
        /Applications/Ghostty.app/Contents/Resources/ghostty/themes /usr/share/ghostty/themes; do
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

tt_current() {
    grep -E '^theme = ' "$tt_config" | head -1 | sed 's/^theme = //'
}

# `key = value` lines of a ghostty config/theme file into tt_c & co (comments start with #, values are #hex)
tt_read() {
    local line key val
    while IFS= read -r line || [ -n "$line" ]; do
        case "$line" in \#* | "") continue ;; esac
        case "$line" in *=*) ;; *) continue ;; esac
        key="${line%%=*}"
        key="${key//[[:space:]]/}"
        val="${line#*=}"
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
    tt_read "$tt_config" # the config's own colour lines win over the theme, as in ghostty itself
    tt_osc_apply
}

tt_apply() {
    tt_path "$1" >/dev/null || tt_die "no such theme: $1"
    case "$(tt_count "$tt_config" '^theme = ')" in
        1) ;;
        0) tt_die "no active 'theme = ' line in $tt_config" ;;
        *) tt_die "more than one active 'theme = ' line in $tt_config" ;;
    esac
    # shellcheck disable=SC2016 # $ENV{TT_NEW} is for perl
    TT_NEW="$1" tt_rewrite "$tt_config" 's{^theme = .*$}{theme = $ENV{TT_NEW}}m'
    tt_preview "$1"
    printf 'written to %s; other windows: reload_config (cmd+shift+, / ctrl+shift+,)\n' "$tt_config" >&2
}

tt_restore() { tt_osc_reset; }

tt_main "$@"