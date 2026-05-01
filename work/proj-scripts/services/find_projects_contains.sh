#!/usr/bin/env bash
# find_projects_contains.sh - Find projects containing specific text in files with given extensions
# Usage: find_projects_contains.sh "text to search" "ext1,ext2,..."
# Example: find_projects_contains.sh "PaymentService" "java,kt,xml"

set -euo pipefail

if [[ $# -lt 1 ]]; then
  echo "Usage: $0 \"text to search\" [\"ext1,ext2,...\"]"
  echo "Example: $0 \"PaymentService\" \"java,kt,xml\""
  exit 1
fi

SEARCH_TEXT="$1"
EXTENSIONS="${2:-}"

# Build ripgrep type/glob flags from comma-separated extensions
RG_ARGS=()
if [[ -n "$EXTENSIONS" ]]; then
  IFS=',' read -ra EXTS <<< "$EXTENSIONS"
  for ext in "${EXTS[@]}"; do
    ext="${ext## }"  # trim leading space
    ext="${ext%% }"  # trim trailing space
    RG_ARGS+=(--glob "*.${ext}")
  done
fi

# Find all matching files, extract project root (first-level subdirectory)
rg --files-with-matches --no-messages "${RG_ARGS[@]}" -- "$SEARCH_TEXT" . 2>/dev/null \
  | sed -E 's|^\./||' \
  | cut -d'/' -f1 \
  | sort -u
