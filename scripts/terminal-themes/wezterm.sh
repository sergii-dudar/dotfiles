#!/usr/bin/env bash
# Interactive theme picker for wezterm: the themes are wezterm's built-in colour schemes (wezterm.color.get_builtin_
# schemes(), over a thousand; names and backgrounds are dumped once per wezterm version into ~/.cache/terminal-themes).
# Moving in the list rewrites the active `return "NAME"` line of get_color_scheme() in appearance.lua and wezterm
# reloads its config at once, so the preview is exactly the end result: config.colors (background, cursor, selection)
# stays on top of every scheme. Enter keeps the line, Esc restores the file. Works the same inside tmux.
#
#   wezterm.sh                 pick interactively
#   wezterm.sh dark|light      only themes with a dark / light background (also `wezterm.sh dark list`)
#   wezterm.sh list|current    the names / the active scheme
#   wezterm.sh apply NAME      set the scheme without the picker

# shellcheck source=common.sh
. "$(dirname "$0")/common.sh"

tt_name=wezterm
tt_config="${TT_CONFIG:-$HOME/.config/wezterm/appearance.lua}"
tt_lua="$(dirname "$0")/wezterm-schemes.lua"
# the one uncommented `return "..."` of get_color_scheme (the commented alternatives start with --)
tt_line='^[[:space:]]*return "[^"]*"[[:space:]]*$'

tt_bgs() { # "NAME<tab>rrggbb" of every scheme
    local cache ver
    command -v wezterm >/dev/null 2>&1 || tt_die "wezterm not on PATH"
    ver="$(wezterm --version | tr -c 'A-Za-z0-9.-' '_')"
    cache="${XDG_CACHE_HOME:-$HOME/.cache}/terminal-themes/wezterm-schemes-bg-$ver.txt"
    if [ ! -s "$cache" ]; then
        mkdir -p "$(dirname "$cache")"
        # show-keys just loads the Lua config and exits; the schemes come out on the log channel
        WEZTERM_LOG=error wezterm --config-file "$tt_lua" show-keys 2>&1 | sed -n 's/.*TT_SCHEME\t//p' | sort >"$cache"
        [ -s "$cache" ] || tt_die "could not list wezterm schemes (wezterm --config-file $tt_lua show-keys)"
    fi
    cat "$cache"
}

tt_list() { tt_bgs | cut -f1; }

tt_current() {
    grep -E -- "$tt_line" "$tt_config" | head -1 | sed -E 's/^[[:space:]]*return "([^"]*)".*/\1/'
}

tt_apply() {
    case "$1" in *'"'* | *\\*) tt_die "scheme name with a quote or backslash: $1" ;; esac
    case "$(tt_count "$tt_config" "$tt_line")" in
        1) ;;
        0) tt_die "no active 'return \"...\"' line in $tt_config" ;;
        *) tt_die "more than one active 'return \"...\"' line in $tt_config" ;;
    esac
    # shellcheck disable=SC2016 # $ENV{TT_NEW} is for perl
    TT_NEW="$1" tt_rewrite "$tt_config" 's{^([ \t]*return ")[^"]*("[ \t]*)$}{$1$ENV{TT_NEW}$2}m'
}

tt_preview() { tt_apply "$1"; }

tt_restore() { :; } # the file is already back, wezterm reloads it on its own

tt_main "$@"