#!/bin/bash
# Push all committed changes on the current (or given) branch to origin via the
# GitHub Git Data API, replaying each local commit one-by-one. Bypasses the
# Zscaler block on git-push (HTTPS pack protocol).
#
# Usage:
#   TOKEN=$(GH_HOST=github.com gh auth token) ./gh-push.sh [branch]
#
# Branch defaults to the current local branch. `origin` is used to derive the
# GitHub repo and default base branch. Every commit between the remote tip (or
# merge-base with the default branch, for new branches) and HEAD is replicated
# on the remote preserving message, author, committer, and timestamps — so the
# resulting remote commit SHAs match your local SHAs and `git fetch` reports
# no divergence. Adds, modifies, deletes, and renames are handled. Merge
# commits are flattened to linear commits using the first-parent diff.

set -euo pipefail

: "${TOKEN:?TOKEN env var required (github.com PAT, repo scope)}"

BRANCH="${1:-$(git rev-parse --abbrev-ref HEAD)}"
[[ "$BRANCH" == "HEAD" ]] && { echo "detached HEAD: pass a branch name explicitly" >&2; exit 2; }

REMOTE_URL=$(git remote get-url origin)
REPO=$(printf '%s\n' "$REMOTE_URL" \
  | sed -E 's|^[^@]+@github\.com:|https://github.com/|; s|\.git/?$||; s|^.*github\.com/||')
[[ -z "$REPO" || "$REPO" == "$REMOTE_URL" ]] && {
  echo "could not parse owner/repo from origin: $REMOTE_URL" >&2; exit 1
}

BASE=$(git symbolic-ref --short refs/remotes/origin/HEAD 2>/dev/null | sed 's|^origin/||')
BASE="${BASE:-main}"

echo "repo:    ${REPO}"
echo "branch:  ${BRANCH} (base: ${BASE})"

API="https://api.github.com/repos/${REPO}"
H_AUTH="Authorization: token ${TOKEN}"
H_ACC="Accept: application/vnd.github+json"
http() { curl -fsS -H "$H_AUTH" -H "$H_ACC" "$@"; }
jq_py() { python3 -c "import sys,json; d=json.load(sys.stdin); $1"; }

# 1) Remote parent: existing branch tip, or default-branch tip for a new branch
PARENT_SHA=$(http "${API}/git/ref/heads/${BRANCH}" 2>/dev/null \
  | jq_py "print(d['object']['sha'])" || true)
NEW_BRANCH=0
if [[ -z "$PARENT_SHA" ]]; then
  echo "branch ${BRANCH} not on remote; will create from ${BASE}"
  PARENT_SHA=$(http "${API}/git/ref/heads/${BASE}" | jq_py "print(d['object']['sha'])")
  NEW_BRANCH=1
fi
PARENT_TREE=$(http "${API}/git/commits/${PARENT_SHA}" | jq_py "print(d['tree']['sha'])")
echo "parent:  ${PARENT_SHA}"

# 2) Local base to diff from
if git cat-file -e "${PARENT_SHA}^{commit}" 2>/dev/null; then
  LOCAL_BASE="$PARENT_SHA"
else
  git fetch --quiet origin "${BRANCH}" 2>/dev/null || true
  if git cat-file -e "${PARENT_SHA}^{commit}" 2>/dev/null; then
    LOCAL_BASE="$PARENT_SHA"
  else
    LOCAL_BASE=$(git merge-base HEAD "origin/${BASE}" 2>/dev/null \
      || git merge-base HEAD "${BASE}")
  fi
fi

# 3) Commits to replay (chronological, oldest first)
COMMITS=$(git rev-list --reverse "${LOCAL_BASE}..HEAD")
if [[ -z "$COMMITS" ]]; then
  echo "no commits to push"; exit 0
fi
COUNT=$(printf '%s\n' "$COMMITS" | wc -l | tr -d ' ')
echo "replaying ${COUNT} commit(s) from ${LOCAL_BASE}"

CURRENT_COMMIT=""
TREE_ENTRIES=()

add_blob() {
  local p="$1" mode content body sha
  mode=$(git ls-tree "$CURRENT_COMMIT" -- "$p" | awk '{print $1}')
  [[ -z "$mode" ]] && mode="100644"
  content=$(git show "${CURRENT_COMMIT}:${p}" | base64 | tr -d '\n')
  body=$(python3 -c "import json,sys;print(json.dumps({'content':sys.argv[1],'encoding':'base64'}))" "$content")
  sha=$(http -X POST "${API}/git/blobs" -d "$body" | jq_py "print(d['sha'])")
  echo "    + ${mode}  ${sha}  ${p}"
  TREE_ENTRIES+=("{\"path\":\"${p}\",\"mode\":\"${mode}\",\"type\":\"blob\",\"sha\":\"${sha}\"}")
}

add_delete() {
  local p="$1"
  echo "    - delete                                             ${p}"
  TREE_ENTRIES+=("{\"path\":\"${p}\",\"mode\":\"100644\",\"type\":\"blob\",\"sha\":null}")
}

PREV_SHA="$PARENT_SHA"
PREV_TREE="$PARENT_TREE"

for COMMIT in $COMMITS; do
  CURRENT_COMMIT="$COMMIT"
  TREE_ENTRIES=()

  SHORT=$(git rev-parse --short "$COMMIT")
  SUBJECT=$(git log -1 --format='%s' "$COMMIT")
  echo
  echo "  ${SHORT}  ${SUBJECT}"

  while IFS= read -r LINE; do
    [[ -z "$LINE" ]] && continue
    STATUS="${LINE%%$'\t'*}"
    REST="${LINE#*$'\t'}"
    case "$STATUS" in
      D) add_delete "$REST" ;;
      R*|C*)
        OLD="${REST%%$'\t'*}"
        NEW="${REST#*$'\t'}"
        [[ "$STATUS" == R* ]] && add_delete "$OLD"
        add_blob "$NEW"
        ;;
      *) add_blob "$REST" ;;
    esac
  done < <(git diff --name-status -M -C "${COMMIT}^" "$COMMIT")

  if [[ ${#TREE_ENTRIES[@]} -eq 0 ]]; then
    NEW_TREE="$PREV_TREE"
    echo "    (no file changes — reusing parent tree)"
  else
    TREE_JSON="["$(IFS=,; echo "${TREE_ENTRIES[*]}")"]"
    TREE_BODY=$(python3 -c "import json,sys;print(json.dumps({'base_tree':sys.argv[1],'tree':json.loads(sys.argv[2])}))" \
      "$PREV_TREE" "$TREE_JSON")
    NEW_TREE=$(http -X POST "${API}/git/trees" -d "$TREE_BODY" | jq_py "print(d['sha'])")
  fi

  MSG=$(git log -1 --format='%B' "$COMMIT")
  AN=$(git log -1 --format='%an' "$COMMIT")
  AE=$(git log -1 --format='%ae' "$COMMIT")
  AD=$(git log -1 --format='%aI' "$COMMIT")
  CN=$(git log -1 --format='%cn' "$COMMIT")
  CE=$(git log -1 --format='%ce' "$COMMIT")
  CD=$(git log -1 --format='%cI' "$COMMIT")

  COMMIT_BODY=$(python3 - "$MSG" "$NEW_TREE" "$PREV_SHA" "$AN" "$AE" "$AD" "$CN" "$CE" "$CD" <<'PY'
import json, sys
m, t, p, an, ae, ad, cn, ce, cd = sys.argv[1:10]
print(json.dumps({
  "message": m,
  "tree": t,
  "parents": [p],
  "author": {"name": an, "email": ae, "date": ad},
  "committer": {"name": cn, "email": ce, "date": cd},
}))
PY
)
  NEW_COMMIT=$(http -X POST "${API}/git/commits" -d "$COMMIT_BODY" | jq_py "print(d['sha'])")

  if [[ "$NEW_COMMIT" == "$COMMIT" ]]; then
    echo "    -> ${NEW_COMMIT} (SHA matches local)"
  else
    echo "    -> ${NEW_COMMIT} (local was ${COMMIT})"
  fi

  PREV_SHA="$NEW_COMMIT"
  PREV_TREE="$NEW_TREE"
done

# 4) Update ref to the final commit
echo
if [[ $NEW_BRANCH -eq 1 ]]; then
  REF_BODY=$(python3 -c "import json,sys;print(json.dumps({'ref':sys.argv[1],'sha':sys.argv[2]}))" \
    "refs/heads/${BRANCH}" "$PREV_SHA")
  http -X POST "${API}/git/refs" -d "$REF_BODY" \
    | jq_py "print('created ${BRANCH} -> '+d['object']['sha'])"
else
  REF_BODY=$(python3 -c "import json,sys;print(json.dumps({'sha':sys.argv[1]}))" "$PREV_SHA")
  http -X PATCH "${API}/git/refs/heads/${BRANCH}" -d "$REF_BODY" \
    | jq_py "print('${BRANCH} -> '+d['object']['sha'])"
fi

echo "done: https://github.com/${REPO}/commits/${BRANCH}"
