#!/usr/bin/env bash
# Prune stale JDTLS workspaces created by the Neovim config.
#
# Every Java project gets its own `-data` workspace and `-configuration` dir under
# ~/.cache/nvim/jdtls/<project>/ (see nvim/.config/nvim/lua/plugins/editor/java/jdtls-config.lua).
# Each holds a JDT index of a few hundred MB, so dozens of projects add up to many GB.
# A workspace's last use is the mtime of its `.metadata/.log` (jdtls rewrites it every session).
#
# Usage: jdtls_workspace_prune.sh [-d DAYS] [-y] [-q]
#   -d DAYS  consider a workspace stale when unused for more than DAYS days (default 60)
#   -y       delete; without it the script only lists what it would remove (dry run)
#   -q       quiet: print only the summary line
#
# Workspaces of a running jdtls (its `-data` path appears in a java process command line) are
# never touched, whatever their age.
set -euo pipefail

CACHE_DIR="${JDTLS_CACHE_DIR:-$HOME/.cache/nvim/jdtls}"
DAYS=60
APPLY=0
QUIET=0

usage() {
    sed -n '2,15p' "$0" | sed 's/^# \{0,1\}//'
}

while getopts ":d:yqh" opt; do
    case "$opt" in
        d) DAYS="$OPTARG" ;;
        y) APPLY=1 ;;
        q) QUIET=1 ;;
        h)
            usage
            exit 0
            ;;
        *)
            usage >&2
            exit 2
            ;;
    esac
done

if [ ! -d "$CACHE_DIR" ]; then
    echo "no jdtls cache dir at $CACHE_DIR" >&2
    exit 0
fi

# `-data <workspace>` arguments of the jdtls JVMs that are running right now
# shellcheck disable=SC2009 # pgrep cannot print the arguments on macOS; the -data path is needed
running_workspaces="$(ps -axo command= 2>/dev/null | grep -F 'org.eclipse.equinox.launcher' | grep -F -- '-data ' \
    | sed -E 's/.* -data ([^ ]+).*/\1/' || true)"

is_running() {
    local ws="$1/workspace"
    [ -n "$running_workspaces" ] && printf '%s\n' "$running_workspaces" | grep -Fxq -- "$ws"
}

human() {
    du -sh "$1" 2>/dev/null | cut -f1
}

now=$(date +%s)
stale_total=0
stale_count=0
kept=0

for dir in "$CACHE_DIR"/*/; do
    dir="${dir%/}"
    [ -d "$dir" ] || continue
    name="$(basename "$dir")"
    marker="$dir/workspace/.metadata/.log"
    if [ -f "$marker" ]; then
        last=$(stat -f %m "$marker" 2>/dev/null || stat -c %Y "$marker")
    else
        # never started or wiped: fall back to the dir itself
        last=$(stat -f %m "$dir" 2>/dev/null || stat -c %Y "$dir")
    fi
    age_days=$(((now - last) / 86400))

    if is_running "$dir"; then
        [ "$QUIET" -eq 1 ] || echo "keep    $name (running)"
        kept=$((kept + 1))
        continue
    fi
    if [ "$age_days" -le "$DAYS" ]; then
        kept=$((kept + 1))
        continue
    fi

    size_kb=$(du -sk "$dir" 2>/dev/null | cut -f1)
    stale_total=$((stale_total + size_kb))
    stale_count=$((stale_count + 1))
    if [ "$APPLY" -eq 1 ]; then
        [ "$QUIET" -eq 1 ] || echo "remove  $name (${age_days}d, $(human "$dir"))"
        rm -rf "$dir"
    else
        [ "$QUIET" -eq 1 ] || echo "stale   $name (${age_days}d, $(human "$dir"))"
    fi
done

action="would remove"
[ "$APPLY" -eq 1 ] && action="removed"
printf '%s %d workspace(s), %d MB; kept %d (unused > %d days = stale; run with -y to delete)\n' \
    "$action" "$stale_count" $((stale_total / 1024)) "$kept" "$DAYS"
