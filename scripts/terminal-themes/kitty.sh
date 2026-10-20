#!/usr/bin/env bash
# Interactive theme picker for kitty: the themes are the *.conf colour files in ~/.config/kitty (vscode-dark.conf,
# GitHub Dark.conf, ...) plus the kitty-themes repo once `kitten themes` has downloaded it: kitty-themes.zip in kitty's
# cache dir (~/Library/Caches/kitty on macOS, ~/.cache/kitty elsewhere), read in place with unzip. Repo themes are
# listed by file name (Alabaster_Dark), not by their `## name:` (Alabaster Dark), so names stay unique.
# Moving in the list points the active `include ...conf` line of kitty.conf at the theme (repo themes are copied to
# current-theme.conf first, like `kitten themes` does) and sends SIGUSR1, on which kitty reloads its config in every
# window, so the preview is exactly the end result: the cursor/selection lines kitty.conf sets after the include stay on
# top. Enter keeps it, Esc restores kitty.conf (and current-theme.conf) and reloads again. Works the same inside tmux.
# `kitten themes` itself previews live too, but not from inside tmux, and it writes between the BEGIN/END_KITTY_THEME
# markers while the include that counts sits after them.
#
#   kitty.sh                 pick interactively
#   kitty.sh dark|light      only themes with a dark / light background (also `kitty.sh dark list`)
#   kitty.sh list|current    the names / the active theme
#   kitty.sh apply NAME      set the theme without the picker

# shellcheck source=common.sh
. "$(dirname "$0")/common.sh"

tt_name=kitty
tt_config="${TT_CONFIG:-$HOME/.config/kitty/kitty.conf}"
tt_dir="$(dirname "$tt_config")"
# kitty's own cache_dir(): KITTY_CACHE_DIRECTORY, else ~/Library/Caches/kitty on macOS, else $XDG_CACHE_HOME/kitty
if [ -n "${KITTY_CACHE_DIRECTORY:-}" ]; then
    tt_cache="$KITTY_CACHE_DIRECTORY"
elif [ "$(uname -s)" = Darwin ]; then
    tt_cache="$HOME/Library/Caches/kitty"
else
    tt_cache="${XDG_CACHE_HOME:-$HOME/.cache}/kitty"
fi
tt_zip="$tt_cache/kitty-themes.zip"
tt_line='^include [^#]+\.conf[[:space:]]*$'

tt_local() { # colour files next to kitty.conf, by name
    find -H "$tt_dir" -maxdepth 1 -name '*.conf' ! -name kitty.conf ! -name current-theme.conf -exec grep -l -E '^color0 ' {} + 2>/dev/null \
        | sed 's|.*/||; s|\.conf$||'
}

tt_repo() { # kitty-themes files in the zip, by file name (empty until `kitten themes` has fetched the repo)
    [ -f "$tt_zip" ] && unzip -Z1 "$tt_zip" '*/themes/*.conf' 2>/dev/null | sed 's|.*/||; s|\.conf$||'
    return 0
}

tt_bgs() { # local files first, so they win over a repo theme of the same name
    local n
    local -a files=()
    while IFS= read -r n; do files+=("$tt_dir/$n.conf"); done < <(tt_local)
    [ ${#files[@]} -eq 0 ] || TT_EXT=.conf tt_bg_scan "${files[@]}"
    [ -f "$tt_zip" ] && unzip -c "$tt_zip" '*/themes/*.conf' 2>/dev/null | TT_EXT=.conf TT_ZIP=1 tt_bg_scan
    return 0
}

tt_repo_cat() { # the repo theme $1 on stdout; fails when the zip has no such theme
    local out
    [ -f "$tt_zip" ] || return 1
    out="$(unzip -p "$tt_zip" "*/themes/$1.conf" 2>/dev/null)" && [ -n "$out" ] || return 1
    printf '%s\n' "$out"
}

tt_list() {
    [ -n "$(tt_repo)" ] || printf '%s\n' "no $tt_zip, local themes only (run \"kitten themes\" once to fetch it)" >&2
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
    local inc theme
    case "$(tt_count "$tt_config" "$tt_line")" in
        1) ;;
        0) tt_die "no active 'include ...conf' line in $tt_config" ;;
        *) tt_die "more than one active 'include ...conf' line in $tt_config" ;;
    esac
    # a local colour file wins over a repo theme of the same name; tt_local leaves out kitty.conf and
    # current-theme.conf, which would include themselves
    if tt_local | grep -q -x -F -- "$1"; then
        inc="$1.conf"
    else
        theme="$(tt_repo_cat "$1")" || tt_die "no such theme: $1"
        inc='current-theme.conf'
        # `## name:` becomes the list name, so tt_current finds the entry again
        {
            printf '## name: %s\n' "$1"
            printf '%s\n' "$theme" | grep -v '^## name: '
        } >"$tt_dir/current-theme.conf"
    fi
    # shellcheck disable=SC2016 # $ENV{TT_NEW} is for perl
    TT_NEW="$inc" tt_rewrite "$tt_config" 's{^include [^#]+\.conf[ \t]*$}{include $ENV{TT_NEW}}m'
    tt_reload
}

tt_preview() { tt_apply "$1"; }

tt_restore() { tt_reload; } # kitty.conf is already back; current-theme.conf may hold the last preview, harmless

tt_main "$@"