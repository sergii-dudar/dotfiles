#!/usr/bin/env bash
# collect_and_push_changes.sh
# Usage: ./collect_and_push_changes.sh [branch-name] [--message "commit msg"]
#
# For every git repo directly under CWD:
#   • If <branch-name> is given and the repo is NOT on it:
#       – create the branch if it doesn't exist, then check it out.
#   • Stage all changes (new files + modifications + deletions).
#   • Commit with the provided message (or a sensible default).
#   • Push to the remote, setting upstream if needed.

set -euo pipefail

# ── colours ────────────────────────────────────────────────────────────────────
RED='\033[0;31m'; GREEN='\033[0;32m'; YELLOW='\033[1;33m'
CYAN='\033[0;36m'; BOLD='\033[1m'; RESET='\033[0m'

info()    { echo -e "${CYAN}[INFO]${RESET}  $*"; }
success() { echo -e "${GREEN}[OK]${RESET}    $*"; }
warn()    { echo -e "${YELLOW}[WARN]${RESET}  $*"; }
error()   { echo -e "${RED}[ERROR]${RESET} $*"; }

# ── argument parsing ───────────────────────────────────────────────────────────
usage() {
  echo -e "${BOLD}Usage:${RESET} $(basename "$0") [branch-name] [--message \"commit message\"]"
  echo
  echo "  branch-name   Optional. If the repo is not on this branch, it will be"
  echo "                created (if absent) and checked out before committing."
  echo "  --message     Commit message (default: 'chore: collect and push changes')."
  exit 1
}

TARGET_BRANCH=""
COMMIT_MSG="chore: collect and push changes"

while [[ $# -gt 0 ]]; do
  case "$1" in
    --message|-m)
      [[ $# -lt 2 ]] && usage
      COMMIT_MSG="$2"; shift 2 ;;
    --help|-h) usage ;;
    --*)       error "Unknown flag: $1"; usage ;;
    *)
      if [[ -z "$TARGET_BRANCH" ]]; then
        TARGET_BRANCH="$1"; shift
      else
        error "Unexpected argument: $1"; usage
      fi ;;
  esac
done

ROOT_DIR="$(pwd)"
SUCCEEDED=()
SKIPPED=()
FAILED=()
PUSHED=()

echo -e "\n${BOLD}Collecting & pushing changes in all git repos under:${RESET} ${ROOT_DIR}"
[[ -n "$TARGET_BRANCH" ]] && echo -e "${BOLD}Target branch:${RESET} ${TARGET_BRANCH}"
echo -e "${BOLD}Commit message:${RESET} ${COMMIT_MSG}\n"

# ── helper: ensure we are on the target branch ────────────────────────────────
ensure_branch() {
  local target="$1"
  local current
  current="$(git rev-parse --abbrev-ref HEAD)"

  if [[ "$current" == "$target" ]]; then
    info "Already on branch '${target}'."
    return 0
  fi

  info "Current branch is '${current}', switching to '${target}'."

  if git show-ref --verify --quiet "refs/heads/${target}"; then
    # Branch exists locally — just check it out
    git checkout "${target}"
  elif git ls-remote --exit-code --heads origin "${target}" &>/dev/null; then
    # Branch exists on remote — track it
    git checkout --track "origin/${target}"
  else
    # Brand new branch
    git checkout -b "${target}"
    info "Created new branch '${target}'."
  fi
}

# ── main loop ─────────────────────────────────────────────────────────────────
while IFS= read -r -d '' git_dir; do
  repo_dir="$(dirname "$git_dir")"
  repo_name="$(basename "$repo_dir")"

  echo -e "${BOLD}── ${repo_name}${RESET}"

  pushd "$repo_dir" > /dev/null

  # Switch branch if requested
  if [[ -n "$TARGET_BRANCH" ]]; then
    if ! ensure_branch "$TARGET_BRANCH"; then
      error "Could not switch to '${TARGET_BRANCH}'. Skipping repo."
      FAILED+=("$repo_name")
      popd > /dev/null
      echo
      continue
    fi
  fi

  CURRENT_BRANCH="$(git rev-parse --abbrev-ref HEAD)"

  # Check for any changes (staged, unstaged, untracked)
  if git diff --quiet && git diff --cached --quiet && [[ -z "$(git ls-files --others --exclude-standard)" ]]; then
    warn "No changes detected — nothing to commit."
    SKIPPED+=("$repo_name")
    popd > /dev/null
    echo
    continue
  fi

  # Stage everything
  git add --all
  info "Staged all changes."

  # Show a compact diff summary
  git diff --cached --stat | sed 's/^/         /'

  # Commit
  if ! git commit -m "${COMMIT_MSG}"; then
    error "Commit failed."
    FAILED+=("$repo_name")
    popd > /dev/null
    echo
    continue
  fi

  # Push — set upstream automatically if this branch has none yet
  REMOTE="origin"
  if git remote get-url "$REMOTE" &>/dev/null; then
    if git push "${REMOTE}" "${CURRENT_BRANCH}" \
         --set-upstream 2>&1 | sed 's/^/         /'; then
      success "Pushed '${CURRENT_BRANCH}' → ${REMOTE}."
      SUCCEEDED+=("$repo_name")
      # Derive a shareable URL from the origin remote (GitHub/GitLab SSH or HTTPS)
      REMOTE_URL="$(git remote get-url "$REMOTE" 2>/dev/null || true)"
      if [[ -n "$REMOTE_URL" ]]; then
        WEB_URL="$(echo "$REMOTE_URL" \
          | sed -E 's#^git@([^:]+):#https://\1/#' \
          | sed -E 's#\.git$##')"
        PUSHED+=("${repo_name}: ${WEB_URL}/tree/${CURRENT_BRANCH}")
      else
        PUSHED+=("${repo_name}: ${CURRENT_BRANCH}")
      fi
    else
      error "Push failed."
      FAILED+=("$repo_name")
    fi
  else
    warn "No remote '${REMOTE}' configured — committed locally only."
    SUCCEEDED+=("$repo_name (local only)")
    PUSHED+=("${repo_name}: ${CURRENT_BRANCH} (local only)")
  fi

  popd > /dev/null
  echo
done < <(find "$ROOT_DIR" -mindepth 2 -maxdepth 2 -name ".git" -print0 | sort -z)

# ── summary ────────────────────────────────────────────────────────────────────
echo -e "${BOLD}══ Summary ══════════════════════════════════════════${RESET}"
echo -e "${GREEN}  Pushed   (${#SUCCEEDED[@]}):${RESET} ${SUCCEEDED[*]:-—}"
echo -e "${YELLOW}  Skipped  (${#SKIPPED[@]}):${RESET} ${SKIPPED[*]:-—}"
echo -e "${RED}  Failed   (${#FAILED[@]}):${RESET} ${FAILED[*]:-—}"
echo

if [[ ${#PUSHED[@]} -gt 0 ]]; then
  echo -e "${BOLD}══ Pushed branches ══════════════════════════════════${RESET}"
  for p in "${PUSHED[@]}"; do
    echo "$p"
  done
  echo
fi

[[ ${#FAILED[@]} -gt 0 ]] && exit 1 || exit 0
