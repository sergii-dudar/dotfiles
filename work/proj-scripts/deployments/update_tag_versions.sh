#!/usr/bin/env bash
#
# Sync deployment image tags between environments for one or more services.
#
# Usage:
#   ./update_tag_versions.sh "dev->uat"  "svc1,svc2,..."
#   ./update_tag_versions.sh "uat->prod" "svc1,svc2,..."
#
# For each service it:
#   1. Reads spec.source.helm.parameters[name=deployment.image.tag].value
#      from <source>/workload/<svc>/<svc>.yaml (any *.yaml except *-vault.yaml).
#   2. Writes the same value into <target>/workload/<svc>/<svc>.yaml.
#   3. Skips and reports if the tag in target already matches source.

set -euo pipefail

if [[ $# -ne 2 ]]; then
  echo "Usage: $0 \"<source>-><target>\" \"svc1,svc2,...\"" >&2
  echo "Example: $0 \"dev->uat\" \"payment-activation,payment-initiation\"" >&2
  exit 1
fi

direction="$1"
services_csv="$2"

if [[ ! "$direction" =~ ^([a-zA-Z0-9_-]+)-\>([a-zA-Z0-9_-]+)$ ]]; then
  echo "Error: direction must look like 'dev->uat' or 'uat->prod', got: '$direction'" >&2
  exit 1
fi
src_env="${BASH_REMATCH[1]}"
tgt_env="${BASH_REMATCH[2]}"

allowed_envs=(dev uat prod)
is_env() {
  local e="$1"
  for a in "${allowed_envs[@]}"; do
    [[ "$a" == "$e" ]] && return 0
  done
  return 1
}
is_env "$src_env" || { echo "Error: unknown source env '$src_env' (allowed: ${allowed_envs[*]})" >&2; exit 1; }
is_env "$tgt_env" || { echo "Error: unknown target env '$tgt_env' (allowed: ${allowed_envs[*]})" >&2; exit 1; }
[[ "$src_env" == "$tgt_env" ]] && { echo "Error: source and target envs are the same" >&2; exit 1; }

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Find the non-vault yaml for a service directory. Fails if 0 or >1 match.
find_service_yaml() {
  local dir="$1"
  local matches=()
  while IFS= read -r -d '' f; do
    matches+=("$f")
  done < <(find "$dir" -maxdepth 1 -type f -name '*.yaml' ! -name '*vault.yaml' -print0 2>/dev/null)
  if [[ ${#matches[@]} -eq 0 ]]; then
    echo ""
    return 1
  fi
  if [[ ${#matches[@]} -gt 1 ]]; then
    echo "Error: multiple non-vault yaml files in $dir: ${matches[*]}" >&2
    return 2
  fi
  echo "${matches[0]}"
}

# Extract deployment.image.tag value from a yaml file.
extract_tag() {
  local file="$1"
  awk '
    /^[[:space:]]*-[[:space:]]+name:[[:space:]]+deployment\.image\.tag[[:space:]]*$/ {
      found = 1
      next
    }
    found && /^[[:space:]]*value:[[:space:]]*/ {
      sub(/^[[:space:]]*value:[[:space:]]*/, "")
      gsub(/^["'\'']|["'\'']$/, "")
      print
      exit
    }
  ' "$file"
}

# Replace deployment.image.tag value in-place.
replace_tag() {
  local file="$1"
  local new_tag="$2"
  local tmp
  tmp="$(mktemp)"
  awk -v new="$new_tag" '
    BEGIN { pending = 0 }
    {
      if (pending && $0 ~ /^[[:space:]]*value:[[:space:]]*/) {
        match($0, /^[[:space:]]*value:[[:space:]]*/)
        prefix = substr($0, 1, RLENGTH)
        print prefix new
        pending = 0
        next
      }
      if ($0 ~ /^[[:space:]]*-[[:space:]]+name:[[:space:]]+deployment\.image\.tag[[:space:]]*$/) {
        pending = 1
      }
      print
    }
  ' "$file" > "$tmp"
  mv "$tmp" "$file"
}

IFS=',' read -r -a services <<< "$services_csv"

updated=0
skipped=0
failed=0
changes=()

echo "Syncing tags: ${src_env} -> ${tgt_env}"
echo

for raw_svc in "${services[@]}"; do
  svc="$(echo "$raw_svc" | xargs)" # trim whitespace
  [[ -z "$svc" ]] && continue

  src_dir="${repo_root}/${src_env}/workload/${svc}"
  tgt_dir="${repo_root}/${tgt_env}/workload/${svc}"

  if [[ ! -d "$src_dir" ]]; then
    echo "[FAIL] ${svc}: source dir not found: $src_dir" >&2
    failed=$((failed + 1))
    continue
  fi
  if [[ ! -d "$tgt_dir" ]]; then
    echo "[FAIL] ${svc}: target dir not found: $tgt_dir" >&2
    failed=$((failed + 1))
    continue
  fi

  src_file="$(find_service_yaml "$src_dir")" || {
    echo "[FAIL] ${svc}: no non-vault yaml in $src_dir" >&2
    failed=$((failed + 1))
    continue
  }
  tgt_file="$(find_service_yaml "$tgt_dir")" || {
    echo "[FAIL] ${svc}: no non-vault yaml in $tgt_dir" >&2
    failed=$((failed + 1))
    continue
  }

  src_tag="$(extract_tag "$src_file")"
  tgt_tag="$(extract_tag "$tgt_file")"

  if [[ -z "$src_tag" ]]; then
    echo "[FAIL] ${svc}: deployment.image.tag not found in $src_file" >&2
    failed=$((failed + 1))
    continue
  fi
  if [[ -z "$tgt_tag" ]]; then
    echo "[FAIL] ${svc}: deployment.image.tag not found in $tgt_file" >&2
    failed=$((failed + 1))
    continue
  fi

  if [[ "$src_tag" == "$tgt_tag" ]]; then
    echo "[SKIP] ${svc}: already in sync at '${src_tag}'"
    skipped=$((skipped + 1))
    continue
  fi

  replace_tag "$tgt_file" "$src_tag"
  echo "[OK]   ${svc}: ${tgt_tag} -> ${src_tag}  (${tgt_file#$repo_root/})"
  updated=$((updated + 1))
  changes+=("${svc}|${src_tag}|${tgt_tag}")
done

echo
echo "Done. updated=${updated} skipped=${skipped} failed=${failed}"

if [[ ${#changes[@]} -gt 0 ]]; then
  echo
  echo "Changed tags:"
  for c in "${changes[@]}"; do
    IFS='|' read -r svc new_tag old_tag <<< "$c"
    echo "  ${svc}: ${src_env} ${new_tag} -> ${tgt_env} ${new_tag} (was ${old_tag})"
  done
fi

[[ $failed -gt 0 ]] && exit 2 || exit 0
