#!/usr/bin/env bash
# show_repo_changes.sh
# Walk immediate subdirectories of $PWD (or a given root) and show changes
# per git repo as full diffs.
#
# Usage:
#   show_repo_changes.sh [-l|--local]      [path]   # default: only local
#   show_repo_changes.sh  -a|--all         [path]   # local + committed
#   show_repo_changes.sh  -c|--committed   [path]   # only committed
#
#   path defaults to $PWD. Only immediate subdirectories that contain a
#   .git/ are considered.
#
# Definitions:
#   local      = uncommitted changes (working tree + index) vs HEAD,
#                plus a list of untracked files.
#   committed  = commits on the current branch not on its upstream (@{u}).
#                Skipped silently for repos with no upstream configured.
#
# Options:
#   --no-color   Disable ANSI colors
#   -h, --help   Show this help

set -euo pipefail

MODE=local        # local | all | committed
ROOT="$PWD"
USE_COLOR=auto

usage() { sed -n '2,22p' "$0" | sed 's/^# \{0,1\}//'; }

while (($#)); do
  case "$1" in
    -l|--local)     MODE=local; shift ;;
    -a|--all)       MODE=all; shift ;;
    -c|--committed) MODE=committed; shift ;;
    --no-color)     USE_COLOR=never; shift ;;
    -h|--help)      usage; exit 0 ;;
    --)             shift; [[ $# -gt 0 ]] && ROOT="$1"; shift || true ;;
    -*)             echo "Unknown option: $1" >&2; usage >&2; exit 2 ;;
    *)              ROOT="$1"; shift ;;
  esac
done

if [[ ! -d "$ROOT" ]]; then
  echo "Not a directory: $ROOT" >&2
  exit 2
fi

if [[ "$USE_COLOR" == "never" ]] || { [[ "$USE_COLOR" == "auto" ]] && [[ ! -t 1 ]]; }; then
  BOLD=''; CYAN=''; GREEN=''; YELLOW=''; GREY=''; RED=''; RESET=''
  GIT_COLOR=never
else
  BOLD=$'\033[1m'; CYAN=$'\033[0;36m'; GREEN=$'\033[0;32m'
  YELLOW=$'\033[1;33m'; GREY=$'\033[0;90m'; RED=$'\033[0;31m'; RESET=$'\033[0m'
  GIT_COLOR=always
fi

# Returns 0 if the current repo has any uncommitted change or untracked file.
has_local_changes() {
  if ! git diff --quiet HEAD 2>/dev/null; then return 0; fi
  if ! git diff --quiet --cached HEAD 2>/dev/null; then return 0; fi
  [[ -n "$(git ls-files --others --exclude-standard 2>/dev/null)" ]]
}

# Print full local diff (working tree + index vs HEAD), then list untracked files.
print_local() {
  if ! git diff --quiet HEAD 2>/dev/null || ! git diff --quiet --cached HEAD 2>/dev/null; then
    git -c color.ui="$GIT_COLOR" diff HEAD
  fi
  local untracked
  untracked=$(git ls-files --others --exclude-standard 2>/dev/null || true)
  if [[ -n "$untracked" ]]; then
    printf '\n%s── untracked ──%s\n' "$YELLOW" "$RESET"
    while IFS= read -r f; do
      printf '%s+ %s%s\n' "$GREEN" "$f" "$RESET"
    done <<<"$untracked"
  fi
}

# Returns 0 if @{u} exists and HEAD is ahead of it.
has_committed_changes() {
  local upstream count
  upstream=$(git rev-parse --abbrev-ref --symbolic-full-name '@{u}' 2>/dev/null) || return 1
  count=$(git rev-list --count "$upstream"..HEAD 2>/dev/null || echo 0)
  (( count > 0 ))
}

# Print full log -p of commits ahead of @{u}.
print_committed() {
  local upstream
  upstream=$(git rev-parse --abbrev-ref --symbolic-full-name '@{u}' 2>/dev/null) || return 0
  git -c color.ui="$GIT_COLOR" log -p --no-merges "$upstream"..HEAD
}

shopt -s nullglob
any=0
for dir in "$ROOT"/*/; do
  [[ -d "${dir}.git" || -f "${dir}.git" ]] || continue
  name=$(basename "$dir")

  # Subshell so cd is scoped, and exit code reports whether anything printed.
  if (
    cd "$dir"
    printed=0
    print_header() {
      (( printed )) && return
      printf '\n%s══ %s ══%s\n' "$BOLD" "$name" "$RESET"
      branch=$(git rev-parse --abbrev-ref HEAD 2>/dev/null || echo "?")
      printf '%s  branch: %s%s\n' "$GREY" "$branch" "$RESET"
      printed=1
    }

    case "$MODE" in
      local|all)
        if has_local_changes; then
          print_header
          printf '%s── local (uncommitted) ──%s\n' "$CYAN" "$RESET"
          print_local
        fi
        ;;
    esac
    case "$MODE" in
      committed|all)
        if has_committed_changes; then
          print_header
          upstream=$(git rev-parse --abbrev-ref --symbolic-full-name '@{u}' 2>/dev/null || echo '@{u}')
          printf '%s── committed (ahead of %s) ──%s\n' "$CYAN" "$upstream" "$RESET"
          print_committed
        fi
        ;;
    esac

    exit $(( printed ? 0 : 1 ))
  ); then
    any=1
  fi
done

if (( ! any )); then
  printf '%sNo changes found in any repo under %s.%s\n' "$GREY" "$ROOT" "$RESET"
fi
