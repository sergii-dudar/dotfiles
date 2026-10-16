#!/usr/bin/env bash
# Interactive theme picker for kitty: the themes are the *.conf colour files in ~/.config/kitty (vscode-dark.conf,
# GitHub Dark.conf, ...) plus the kitty-themes repo once `kitten themes` has downloaded it to ~/.cache/kitty/themes.
# Moving in the list points the active `include ...conf` line of kitty.conf at the theme (repo themes are copied to
# current-theme.conf first, like `kitten themes` does) and sends SIGUSR1, on which kitty reloads its config in every
# window, so the preview is exactly the end result: the cursor/selection lines kitty.conf sets after the include stay on
# top. Enter keeps it, Esc restores kitty.conf (and current-theme.conf) and reloads again. Works the same inside tmux.
# `kitten themes` itself previews live too, but not from inside tmux, and it writes between the BEGIN/END_KITTY_THEME
# markers while the include that counts sits after them.
#
#   kitty.sh                 pick interactively
#   kitty.sh list|current    the names / the active theme
#   kitty.sh apply NAME      set the theme without the picker

# shellcheck source=common.sh
. "$(dirname "$0")/common.sh"

tt_name=kitty
tt_config="${TT_CONFIG:-$HOME/.config/kitty/kitty.conf}"
tt_dir="$(dirname "$tt_config")"
tt_cache="${XDG_CACHE_HOME:-$HOME/.cache}/kitty/themes"
tt_line='^include [^#]+\.conf[[:space:]]*$'

tt_local() { # colour files next to kitty.conf, by name
    find "$tt_dir" -maxdepth 1 -name '*.conf' ! -name kitty.conf ! -name current-theme.conf -exec grep -l -E '^color0 ' {} + 2>/dev/null \
        | sed 's|.*/||; s|\.conf$||'
}

tt_repo() { # kitty-themes files in the cache, by name (empty until `kitten themes` has fetched the repo)
    [ -d "$tt_cache" ] && find "$tt_cache" -name '*.conf' -path '*/themes/*' 2>/dev/null | sed 's|.*/||; s|\.conf$||'
    return 0
}

tt_path() { # a local file wins over a repo file of the same name
    local f
    [ -f "$tt_dir/$1.conf" ] && printf '%s\n' "$tt_dir/$1.conf" && return 0
    f="$([ -d "$tt_cache" ] && find "$tt_cache" -name "$1.conf" -path '*/themes/*' 2>/dev/null | head -1)"
    [ -n "$f" ] && printf '%s\n' "$f" && return 0
    return 1
}

tt_list() {
    [ -n "$(tt_repo)" ] || printf '%s\n' 'kitty-themes repo not cached, local themes only (run "kitten themes" once to fetch it)' >&2
    {
        tt_local
        tt_repo
    } | sort -u
}

tt_current() {
    local inc
    inc="$(grep -E -- "$tt_line" "$tt_config" | head -1 | sed -E 's/^include //; s/[[:space:]]*$//')"
    case "$inc" in
        current-theme.conf) sed -n 's/^## name: //p' "$tt_dir/current-theme.conf" 2>/dev/null | head -1 ;;
        *) printf '%s\n' "${inc%.conf}" ;;
    esac
}

tt_reload() { pkill -USR1 -x kitty 2>/dev/null || true; } # every kitty of this user re-reads its config

tt_apply() {
    local file inc
    file="$(tt_path "$1")" || tt_die "no such theme: $1"
    case "$(tt_count "$tt_config" "$tt_line")" in
        1) ;;
        0) tt_die "no active 'include ...conf' line in $tt_config" ;;
        *) tt_die "more than one active 'include ...conf' line in $tt_config" ;;
    esac
    if [ "$file" = "$tt_dir/$1.conf" ]; then
        inc="$1.conf"
    else
        inc='current-theme.conf'
        {
            grep -q '^## name: ' "$file" || printf '## name: %s\n' "$1"
            cat "$file"
        } >"$tt_dir/current-theme.conf"
    fi
    # shellcheck disable=SC2016 # $ENV{TT_NEW} is for perl
    TT_NEW="$inc" tt_rewrite "$tt_config" 's{^include [^#]+\.conf[ \t]*$}{include $ENV{TT_NEW}}m'
    tt_reload
}

tt_preview() { tt_apply "$1"; }

tt_restore() { tt_reload; } # kitty.conf is already back; current-theme.conf may hold the last preview, harmless

tt_main "$@"