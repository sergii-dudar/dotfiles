#!/usr/bin/env bash
set -euo pipefail

usage() {
    cat <<EOF
Usage: $(basename "$0") <extensions> <source> <target>

Symlink files with given extensions from <source> into <target>.

Arguments:
  extensions  Comma-separated list of extensions (e.g. "sh,java")
  source      Directory to search for files
  target      Directory where symlinks will be created

Example:
  $(basename "$0") sh,java ./scripts ~/bin
EOF
    exit 1
}

[[ $# -eq 3 ]] || usage

extensions="$1"
source_dir="$2"
target_dir="$3"

[[ -d "$source_dir" ]] || { echo "Error: source directory does not exist: $source_dir" >&2; exit 1; }

source_abs="$(cd "$source_dir" && pwd)"

mkdir -p "$target_dir"
target_abs="$(cd "$target_dir" && pwd)"

find_args=()
IFS=',' read -ra ext_arr <<< "$extensions"
first=1
for ext in "${ext_arr[@]}"; do
    ext="${ext#.}"
    ext="${ext// /}"
    [[ -z "$ext" ]] && continue
    if [[ $first -eq 1 ]]; then
        find_args+=(-name "*.${ext}")
        first=0
    else
        find_args+=(-o -name "*.${ext}")
    fi
done

[[ ${#find_args[@]} -gt 0 ]] || { echo "Error: no valid extensions provided" >&2; exit 1; }

count=0
while IFS= read -r -d '' file; do
    link_name="$target_abs/$(basename "$file")"
    ln -sfn "$file" "$link_name"
    echo "linked: $link_name -> $file"
    count=$((count + 1))
done < <(find "$source_abs" -type f \( "${find_args[@]}" \) -print0)

echo "Done. $count file(s) symlinked into $target_abs"
