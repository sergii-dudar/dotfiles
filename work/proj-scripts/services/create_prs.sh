#!/usr/bin/env bash
# create_prs.sh
# Usage: ./create_prs.sh [--base <base-branch>] [--title "PR title"] [--body "PR body"] [--draft]
#
# For every git repo directly under CWD:
#   • Reads the current branch name.
#   • Creates a GitHub Pull Request from that branch into <base-branch> (default: main).
#   • Skips repos that are already on the base branch or have no upstream remote.

set -euo pipefail

# ── colours ────────────────────────────────────────────────────────────────────
RED='\033[0;31m'; GREEN='\033[0;32m'; YELLOW='\033[1;33m'
CYAN='\033[0;36m'; BOLD='\033[1m'; RESET='\033[0m'

info()    { echo -e "${CYAN}[INFO]${RESET}  $*"; }
success() { echo -e "${GREEN}[OK]${RESET}    $*"; }
warn()    { echo -e "${YELLOW}[WARN]${RESET}  $*"; }
error()   { echo -e "${RED}[ERROR]${RESET} $*"; }

# ── preflight: gh must be installed and authenticated ─────────────────────────
if ! command -v gh &>/dev/null; then
  error "'gh' (GitHub CLI) is not installed. See https://cli.github.com"
  exit 1
fi

if ! gh auth status &>/dev/null; then
  error "Not authenticated with GitHub CLI. Run: gh auth login"
  exit 1
fi

# ── argument parsing ───────────────────────────────────────────────────────────
usage() {
  echo -e "${BOLD}Usage:${RESET} $(basename "$0") [options]"
  echo
  echo "Options:"
  echo "  --base,    -b  <branch>   Base branch for the PR       (default: main)"
  echo "  --title,   -t  <text>     PR title                     (default: auto from branch name)"
  echo "  --body,    -B  <text>     PR description body          (default: empty)"
  echo "  --body-file    <file>     Read PR body from a file"
  echo "  --draft,   -d             Open PR as draft"
  echo "  --reviewer -r  <handles>  Comma-separated reviewer handles (e.g. alice,bob)"
  echo "  --label,   -l  <labels>   Comma-separated labels"
  echo "  --help,    -h             Show this help"
  exit 1
}

BASE_BRANCH="main"
PR_TITLE=""
PR_BODY=""
BODY_FILE=""
DRAFT_FLAG=""
REVIEWERS=""
LABELS=""

while [[ $# -gt 0 ]]; do
  case "$1" in
    --base|-b)       [[ $# -lt 2 ]] && usage; BASE_BRANCH="$2";  shift 2 ;;
    --title|-t)      [[ $# -lt 2 ]] && usage; PR_TITLE="$2";     shift 2 ;;
    --body|-B)       [[ $# -lt 2 ]] && usage; PR_BODY="$2";      shift 2 ;;
    --body-file)     [[ $# -lt 2 ]] && usage; BODY_FILE="$2";    shift 2 ;;
    --draft|-d)      DRAFT_FLAG="--draft";                        shift   ;;
    --reviewer|-r)   [[ $# -lt 2 ]] && usage; REVIEWERS="$2";    shift 2 ;;
    --label|-l)      [[ $# -lt 2 ]] && usage; LABELS="$2";       shift 2 ;;
    --help|-h)       usage ;;
    *)               error "Unknown argument: $1"; usage ;;
  esac
done

# Validate body file if provided
if [[ -n "$BODY_FILE" ]]; then
  [[ -f "$BODY_FILE" ]] || { error "Body file not found: ${BODY_FILE}"; exit 1; }
fi

ROOT_DIR="$(pwd)"
SUCCEEDED=()
SKIPPED=()
FAILED=()
PR_URLS=()

echo -e "\n${BOLD}Creating PRs for all git repos under:${RESET} ${ROOT_DIR}"
echo -e "${BOLD}Base branch:${RESET} ${BASE_BRANCH}"
[[ -n "$DRAFT_FLAG" ]] && echo -e "${BOLD}Mode:${RESET} draft"
echo

# ── helper: derive a human-readable title from a branch name ──────────────────
branch_to_title() {
  local branch="$1"
  # Strip common prefixes (feature/, fix/, chore/, etc.) then replace - and _ with spaces
  echo "$branch" \
    | sed 's|^[a-z]*/||'   \
    | sed 's/[-_]/ /g'     \
    | awk '{for(i=1;i<=NF;i++) $i=toupper(substr($i,1,1)) substr($i,2); print}'
}

# ── main loop ─────────────────────────────────────────────────────────────────
while IFS= read -r -d '' git_dir; do
  repo_dir="$(dirname "$git_dir")"
  repo_name="$(basename "$repo_dir")"

  echo -e "${BOLD}── ${repo_name}${RESET}"

  pushd "$repo_dir" > /dev/null

  # ── get current branch ──────────────────────────────────────────────────────
  CURRENT_BRANCH="$(git rev-parse --abbrev-ref HEAD 2>/dev/null || true)"

  if [[ -z "$CURRENT_BRANCH" || "$CURRENT_BRANCH" == "HEAD" ]]; then
    warn "Detached HEAD or unable to read branch — skipping."
    SKIPPED+=("$repo_name (detached HEAD)")
    popd > /dev/null; echo; continue
  fi

  # ── skip if already on base branch ─────────────────────────────────────────
  if [[ "$CURRENT_BRANCH" == "$BASE_BRANCH" ]]; then
    warn "Already on base branch '${BASE_BRANCH}' — nothing to PR into itself."
    SKIPPED+=("$repo_name (on base branch)")
    popd > /dev/null; echo; continue
  fi

  info "Branch: ${CURRENT_BRANCH} → ${BASE_BRANCH}"

  # ── ensure remote exists ────────────────────────────────────────────────────
  if ! git remote get-url origin &>/dev/null; then
    warn "No 'origin' remote — skipping."
    SKIPPED+=("$repo_name (no remote)")
    popd > /dev/null; echo; continue
  fi

  # ── check if remote tracking branch exists (branch must be pushed) ──────────
  if ! git ls-remote --exit-code --heads origin "${CURRENT_BRANCH}" &>/dev/null; then
    warn "Branch '${CURRENT_BRANCH}' not found on remote. Push it first."
    SKIPPED+=("$repo_name (branch not pushed)")
    popd > /dev/null; echo; continue
  fi

  # ── check if a PR already exists for this branch ────────────────────────────
  EXISTING_PR="$(gh pr list --head "${CURRENT_BRANCH}" --base "${BASE_BRANCH}" \
                   --state open --json url --jq '.[0].url' 2>/dev/null || true)"

  if [[ -n "$EXISTING_PR" ]]; then
    warn "Open PR already exists: ${EXISTING_PR}"
    SKIPPED+=("$repo_name (PR exists)")
    PR_URLS+=("$EXISTING_PR")
    popd > /dev/null; echo; continue
  fi

  # ── build PR title ──────────────────────────────────────────────────────────
  TITLE="${PR_TITLE:-$(branch_to_title "$CURRENT_BRANCH")}"

  # ── assemble gh pr create arguments ────────────────────────────────────────
  GH_ARGS=(
    pr create
    --base  "${BASE_BRANCH}"
    --head  "${CURRENT_BRANCH}"
    --title "${TITLE}"
  )

  # Body: file takes precedence over inline text
  if [[ -n "$BODY_FILE" ]]; then
    GH_ARGS+=(--body-file "${BODY_FILE}")
  elif [[ -n "$PR_BODY" ]]; then
    GH_ARGS+=(--body "${PR_BODY}")
  else
    GH_ARGS+=(--body "")
  fi

  [[ -n "$DRAFT_FLAG"  ]] && GH_ARGS+=("$DRAFT_FLAG")

  # Reviewers: split on comma and add one --reviewer flag per handle
  if [[ -n "$REVIEWERS" ]]; then
    IFS=',' read -ra REVIEWER_LIST <<< "$REVIEWERS"
    for r in "${REVIEWER_LIST[@]}"; do
      r="$(echo "$r" | xargs)"   # trim whitespace
      [[ -n "$r" ]] && GH_ARGS+=(--reviewer "$r")
    done
  fi

  # Labels: split on comma and add one --label flag per label
  if [[ -n "$LABELS" ]]; then
    IFS=',' read -ra LABEL_LIST <<< "$LABELS"
    for l in "${LABEL_LIST[@]}"; do
      l="$(echo "$l" | xargs)"
      [[ -n "$l" ]] && GH_ARGS+=(--label "$l")
    done
  fi

  # ── create the PR ───────────────────────────────────────────────────────────
  PR_URL="$(gh "${GH_ARGS[@]}" 2>&1)" && GH_EXIT=0 || GH_EXIT=$?

  if [[ $GH_EXIT -eq 0 ]]; then
    success "PR created: ${PR_URL}"
    SUCCEEDED+=("$repo_name")
    PR_URLS+=("$PR_URL")
  else
    error "gh pr create failed:"
    echo "$PR_URL" | sed 's/^/         /'
    FAILED+=("$repo_name")
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

if [[ ${#PR_URLS[@]} -gt 0 ]]; then
  echo -e "${BOLD}══ Created PR links ═════════════════════════════════${RESET}"
  for url in "${PR_URLS[@]}"; do
    echo "$url"
  done
  echo
fi

[[ ${#FAILED[@]} -gt 0 ]] && exit 1 || exit 0
