#!/usr/bin/env bash
# Read all of stdin into a variable
input=$(cat)

# Extract fields with jq, "// 0" provides fallback for null
# MODEL=$(echo "$input" | jq -r '.model.display_name')
MODEL=$(echo "$input" | jq -r '.model.id')
PCT=$(echo "$input" | jq -r '.context_window.used_percentage // 0' | cut -d. -f1)
DIR=$(echo "$input" | jq -r '.workspace.current_dir')

BRANCH=""
git rev-parse --git-dir > /dev/null 2>&1 && BRANCH=" |  $(git branch --show-current 2>/dev/null)"

BAR=""
[ "$FILLED" -gt 0 ] && printf -v FILL "%${FILLED}s" && BAR="${FILL// /▓}"
[ "$EMPTY" -gt 0 ] && printf -v PAD "%${EMPTY}s" && BAR="${BAR}${PAD// /░}"

# echo "[$MODEL] $BAR $PCT%"
echo -e " $MODEL |   ${DIR##*/}$BRANCH | 󰦘 ${PCT}%"
