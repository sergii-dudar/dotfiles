#!/usr/bin/env bash
# create_branch.sh
# Usage: ./create_branch.sh <branch-name>
# Creates and checks out a new branch in every git repo found directly under CWD.

set -euo pipefail

# ── colours ────────────────────────────────────────────────────────────────────
RED='\033[0;31m'; GREEN='\033[0;32m'; YELLOW='\033[1;33m'
CYAN='\033[0;36m'; BOLD='\033[1m'; RESET='\033[0m'

info()    { echo -e "${CYAN}[INFO]${RESET}  $*"; }
success() { echo -e "${GREEN}[OK]${RESET}    $*"; }
warn()    { echo -e "${YELLOW}[WARN]${RESET}  $*"; }
error()   { echo -e "${RED}[ERROR]${RESET} $*"; }

# ── argument guard ──────────────────────────────────────────────────────────────
if [[ $# -lt 1 ]]; then
  echo -e "${BOLD}Usage:${RESET} $(basename "$0") <branch-name>"
  exit 1
fi

BRANCH="$1"
ROOT_DIR="$(pwd)"
SUCCEEDED=()
SKIPPED=()
FAILED=()
READY_REPOS=()

echo -e "\n${BOLD}Creating branch '${BRANCH}' across all git repos in:${RESET} ${ROOT_DIR}\n"

# ── iterate over every immediate subdirectory that is a git repo ───────────────
while IFS= read -r -d '' git_dir; do
  repo_dir="$(dirname "$git_dir")"
  repo_name="$(basename "$repo_dir")"

  echo -e "${BOLD}── ${repo_name}${RESET}"

  pushd "$repo_dir" > /dev/null

  # Check if branch already exists (locally)
  if git show-ref --verify --quiet "refs/heads/${BRANCH}"; then
    CURRENT_BRANCH="$(git rev-parse --abbrev-ref HEAD 2>/dev/null || true)"
    if [[ "$CURRENT_BRANCH" == "$BRANCH" ]]; then
      success "Already on '${BRANCH}' — verified."
      SKIPPED+=("$repo_name (already on branch)")
      READY_REPOS+=("$repo_name")
    else
      warn "Branch '${BRANCH}' already exists — checking it out instead."
      if git checkout "${BRANCH}"; then
        SKIPPED+=("$repo_name")
        READY_REPOS+=("$repo_name")
      else
        error "Checkout failed."
        FAILED+=("$repo_name")
      fi
    fi
  else
    # Create and checkout the new branch
    if git checkout -b "${BRANCH}"; then
      success "Created and checked out '${BRANCH}'."
      SUCCEEDED+=("$repo_name")
      READY_REPOS+=("$repo_name")
    else
      error "Failed to create branch."
      FAILED+=("$repo_name")
    fi
  fi

  popd > /dev/null
  echo
done < <(find "$ROOT_DIR" -mindepth 2 -maxdepth 2 -name ".git" -print0 | sort -z)

# ── summary ────────────────────────────────────────────────────────────────────
echo -e "${BOLD}══ Summary ══════════════════════════════════════════${RESET}"
echo -e "${GREEN}  Created  (${#SUCCEEDED[@]}):${RESET} ${SUCCEEDED[*]:-—}"
echo -e "${YELLOW}  Skipped  (${#SKIPPED[@]}):${RESET} ${SKIPPED[*]:-—}"
echo -e "${RED}  Failed   (${#FAILED[@]}):${RESET} ${FAILED[*]:-—}"
echo

if [[ ${#READY_REPOS[@]} -gt 0 ]]; then
  echo -e "${BOLD}══ Repos on '${BRANCH}' ═════════════════════════════${RESET}"
  for r in "${READY_REPOS[@]}"; do
    echo "$r"
  done
  echo
fi

[[ ${#FAILED[@]} -gt 0 ]] && exit 1 || exit 0
