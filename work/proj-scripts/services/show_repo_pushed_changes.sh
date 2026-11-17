#!/usr/bin/env bash
# show_repo_pushed_changes.sh
# Walk immediate subdirectories of $PWD (or a given root) and show, per git
# repo, what the pushed state of the currently checked-out branch changes
# compared to the main branch.
#
# Usage:
#   show_repo_pushed_changes.sh [options] [path]
#
#   path defaults to $PWD. Only immediate subdirectories that contain a
#   .git/ are considered.
#
# Definitions:
#   pushed  = the remote-tracking ref of the current branch (its upstream,
#             else origin/<branch>). Local commits that are not pushed yet
#             and uncommitted changes are NOT included.
#   main    = origin/HEAD, else origin/main, else origin/master.
#   changes = diff from the merge base of main and pushed to pushed, i.e.
#             what a PR of this branch into main would show.
#
#   Repos sitting on the main branch, with an unpushed branch, or with no
#   difference to main are skipped silently.
#
# Options:
#   -f, --fetch         Run 'git fetch origin' in each repo first
#   -s, --stat          Show the commit list and a diffstat instead of full diffs
#   -b, --base <ref>    Compare against this branch instead of the detected main
#                       (e.g. -b develop; resolved as origin/<ref> when it exists)
#   --no-color          Disable ANSI colors (and delta rendering)
#   -h, --help          Show this help
#
# Full diffs are rendered with delta when it is installed and colors are on.

set -euo pipefail

ROOT="$PWD"
USE_COLOR=auto
FETCH=0
STAT=0
BASE=""

usage() { sed -n '2,33p' "$0" | sed 's/^# \{0,1\}//'; }

while (($#)); do
    case "$1" in
        -f | --fetch)
            FETCH=1
            shift
            ;;
        -s | --stat)
            STAT=1
            shift
            ;;
        -b | --base)
            [[ $# -ge 2 ]] || {
                echo "Missing value for $1" >&2
                exit 2
            }
            BASE="$2"
            shift 2
            ;;
        --no-color)
            USE_COLOR=never
            shift
            ;;
        -h | --help)
            usage
            exit 0
            ;;
        --)
            shift
            [[ $# -gt 0 ]] && ROOT="$1"
            shift || true
            ;;
        -*)
            echo "Unknown option: $1" >&2
            usage >&2
            exit 2
            ;;
        *)
            ROOT="$1"
            shift
            ;;
    esac
done

if [[ ! -d "$ROOT" ]]; then
    echo "Not a directory: $ROOT" >&2
    exit 2
fi

if [[ "$USE_COLOR" == "never" ]] || { [[ "$USE_COLOR" == "auto" ]] && [[ ! -t 1 ]]; }; then
    BOLD=''
    CYAN=''
    GREY=''
    RED=''
    RESET=''
    GIT_COLOR=never
else
    BOLD=$'\033[1m'
    CYAN=$'\033[0;36m'
    GREY=$'\033[0;90m'
    RED=$'\033[0;31m'
    RESET=$'\033[0m'
    GIT_COLOR=always
fi

USE_DELTA=0
if [[ "$GIT_COLOR" == "always" ]] && command -v delta >/dev/null 2>&1; then
    USE_DELTA=1
fi

# Never let git block on a credential prompt.
export GIT_TERMINAL_PROMPT=0

# Run a diff-producing git command, rendered by delta when available.
show_diff() {
    if ((USE_DELTA)); then
        git -c color.ui=never --no-pager "$@" | delta --paging=never
    else
        git -c color.ui="$GIT_COLOR" --no-pager "$@"
    fi
}

ref_exists() { git rev-parse --verify --quiet "$1^{commit}" >/dev/null; }

# Print the ref to compare against: --base override, else the remote default branch.
resolve_base() {
    local ref
    if [[ -n "$BASE" ]]; then
        for ref in "origin/$BASE" "$BASE"; do
            if ref_exists "$ref"; then
                echo "$ref"
                return 0
            fi
        done
        return 1
    fi
    if ref=$(git symbolic-ref --quiet --short refs/remotes/origin/HEAD 2>/dev/null) && ref_exists "$ref"; then
        echo "$ref"
        return 0
    fi
    for ref in origin/main origin/master; do
        if ref_exists "$ref"; then
            echo "$ref"
            return 0
        fi
    done
    return 1
}

# Print the remote-tracking ref holding the pushed state of the current branch.
resolve_pushed() {
    local branch="$1" ref
    if ref=$(git rev-parse --abbrev-ref --symbolic-full-name '@{u}' 2>/dev/null) && ref_exists "$ref"; then
        echo "$ref"
        return 0
    fi
    if ref_exists "origin/$branch"; then
        echo "origin/$branch"
        return 0
    fi
    return 1
}

shopt -s nullglob
any=0
for dir in "$ROOT"/*/; do
    [[ -d "${dir}.git" || -f "${dir}.git" ]] || continue
    name=$(basename "$dir")

    # Subshell so cd is scoped, and exit code reports whether anything printed.
    if (
        cd "$dir"

        if ((FETCH)) && ! git fetch --quiet origin 2>/dev/null; then
            printf '%s%s: fetch failed, using the last fetched state%s\n' "$RED" "$name" "$RESET" >&2
        fi

        branch=$(git symbolic-ref --quiet --short HEAD 2>/dev/null) || exit 1
        base=$(resolve_base) || exit 1
        pushed=$(resolve_pushed "$branch") || exit 1
        [[ "$pushed" != "$base" ]] || exit 1

        ahead=$(git rev-list --count "$base..$pushed")
        ((ahead > 0)) || exit 1
        behind=$(git rev-list --count "$pushed..$base")
        unpushed=$(git rev-list --count "$pushed..HEAD")

        printf '\n%s══ %s ══%s\n' "$BOLD" "$name" "$RESET"
        printf '%s  branch: %s (%s), %s ahead / %s behind %s%s\n' \
            "$GREY" "$branch" "$pushed" "$ahead" "$behind" "$base" "$RESET"
        if ((unpushed > 0)); then
            printf '%s  %s local commit(s) not pushed yet, not shown%s\n' "$GREY" "$unpushed" "$RESET"
        fi

        printf '%s── commits ──%s\n' "$CYAN" "$RESET"
        git -c color.ui="$GIT_COLOR" --no-pager log --oneline --no-merges "$base..$pushed"

        printf '%s── pushed changes vs %s ──%s\n' "$CYAN" "$base" "$RESET"
        if ((STAT)); then
            git -c color.ui="$GIT_COLOR" --no-pager diff --stat "$base...$pushed"
        else
            show_diff diff "$base...$pushed"
        fi
    ); then
        any=1
    fi
done

if ((! any)); then
    printf '%sNo pushed branch changes found in any repo under %s.%s\n' "$GREY" "$ROOT" "$RESET"
fi
