#!/usr/bin/env bash
# Extract service names from a list of PR or repo URLs, stripping ua-payments-payment- / ua-payments- prefixes.
# Usage: extract_service_names.sh [file]   (reads stdin if no file given)

set -euo pipefail

input_file=""
if [[ $# -ge 1 ]]; then
  input_file="$1"
fi

names=()
while IFS= read -r line; do
  line="${line%%#*}"       # strip inline comments
  line="${line//[$'\t\r ']}"  # trim whitespace
  [[ -z "$line" ]] && continue

  repo=""
  # PR URL: https://host/owner/repo/pull/N
  if [[ "$line" =~ ^https?://[^/]+/[^/]+/([^/]+)/pull/[0-9]+ ]]; then
    repo="${BASH_REMATCH[1]}"
  # Repo URL: https://host/owner/repo (optional trailing slash)
  elif [[ "$line" =~ ^https?://[^/]+/[^/]+/([^/]+)(/?)$ ]]; then
    repo="${BASH_REMATCH[1]}"
  # owner/repo slug
  elif [[ "$line" =~ ^[^/]+/([^/]+)$ ]]; then
    repo="${BASH_REMATCH[1]}"
  else
    repo="$line"
  fi

  name="${repo#ua-payments-}"

  names+=("$name")
done < "${input_file:-/dev/stdin}"

IFS=','; printf '%s\n' "${names[*]}"
