#!/usr/bin/env bash
# copy_projects_contains.sh - Find projects containing text and copy them to a destination
# Usage: copy_projects_contains.sh "text to search" "ext1,ext2,..." /path/to/destination
# Example: copy_projects_contains.sh ">infra-tracing<" "xml" /tmp/filtered

set -euo pipefail

if [[ $# -lt 3 ]]; then
  echo "Usage: $0 \"text to search\" \"extensions\" \"/destination/path\""
  echo "Example: $0 \">infra-tracing<\" \"xml\" /tmp/projects"
  exit 1
fi

SEARCH_TEXT="$1"
EXTENSIONS="$2"
DEST_DIR="$3"

# Build ripgrep glob flags from comma-separated extensions
RG_ARGS=()
if [[ -n "$EXTENSIONS" ]]; then
  IFS=',' read -ra EXTS <<< "$EXTENSIONS"
  for ext in "${EXTS[@]}"; do
    ext="${ext## }"
    ext="${ext%% }"
    RG_ARGS+=(--glob "*.${ext}")
  done
fi

# Find matching projects (first-level subdirectories)
PROJECTS=$(rg --files-with-matches --no-messages "${RG_ARGS[@]}" -- "$SEARCH_TEXT" . 2>/dev/null \
  | sed -E 's|^\./||' \
  | cut -d'/' -f1 \
  | sort -u)

if [[ -z "$PROJECTS" ]]; then
  echo "No projects found containing \"$SEARCH_TEXT\""
  exit 0
fi

echo "Found projects:"
echo "$PROJECTS"
echo ""

# Create destination directory
mkdir -p "$DEST_DIR"

# Copy each project
while IFS= read -r project; do
  if [[ -d "$project" ]]; then
    echo "Copying: $project -> $DEST_DIR/$project"
    cp -a "$project" "$DEST_DIR/"
  fi
done <<< "$PROJECTS"

echo ""
echo "Done. Copied to: $DEST_DIR"
