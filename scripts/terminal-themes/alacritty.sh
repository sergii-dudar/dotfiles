#!/usr/bin/env bash
# Interactive theme picker for alacritty: the themes are the alacritty-theme repo files next to the config
# (~/.config/alacritty/themes/themes/*.toml, see the `import` comment in alacritty.toml). Moving in the list rewrites
# the active theme line of the `import = [...]` block and alacritty's live_config_reload applies it at once, so the
# preview is exactly the end result: the colours alacritty.toml sets itself (background, cursor, selection) stay on top
# of every theme. Enter keeps the line, Esc restores the file. Works the same inside tmux.
#
#   alacritty.sh                 pick interactively
#   alacritty.sh list|current    the names / the active theme
#   alacritty.sh apply NAME      set the theme without the picker

# shellcheck source=common.sh
. "$(dirname "$0")/common.sh"

tt_name=alacritty
tt_config="${TT_CONFIG:-$HOME/.config/alacritty/alacritty.toml}"
tt_dir="$(dirname "$tt_config")/themes/themes"
# the one uncommented theme import: optional spaces, a quoted path ending in /themes/NAME.toml, optional comma
tt_line='^[[:space:]]*"[^"]*/themes/[^"/]+\.toml"[[:space:]]*,?[[:space:]]*$'

tt_list() {
    [ -d "$tt_dir" ] || tt_die "no themes in $tt_dir (git clone https://github.com/alacritty/alacritty-theme $(dirname "$tt_dir"))"
    find -H "$tt_dir" -maxdepth 1 -name '*.toml' | sed 's|.*/||; s|\.toml$||' | sort
}

tt_current() {
    grep -E -- "$tt_line" "$tt_config" | head -1 | sed -E 's|.*/themes/([^"/]+)\.toml".*|\1|'
}

tt_apply() {
    [ -f "$tt_dir/$1.toml" ] || tt_die "no such theme: $1"
    case "$(tt_count "$tt_config" "$tt_line")" in
        1) ;;
        0) tt_die "no active theme import line in $tt_config" ;;
        *) tt_die "more than one active theme import line in $tt_config" ;;
    esac
    # shellcheck disable=SC2016 # $ENV{TT_NEW} is for perl
    TT_NEW="$1" tt_rewrite "$tt_config" 's{^([ \t]*"[^"]*/themes/)[^"/]+(\.toml"[ \t]*,?[ \t]*)$}{$1$ENV{TT_NEW}$2}m'
}

tt_preview() { tt_apply "$1"; }

tt_restore() { :; } # the file is already back, alacritty reloads it on its own

tt_main "$@"