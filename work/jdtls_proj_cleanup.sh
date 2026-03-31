#!/usr/bin/env bash
# Cleans jdtls cache and Eclipse project files from a Maven/Gradle multi-module project.
# Run from the project root directory.

set -euo pipefail

root="$(pwd)"
project_name="$(basename "$root")"
jdtls_cache="$HOME/.cache/nvim/jdtls/$project_name"

echo "Project root: $root"
echo "JDTLS cache:  $jdtls_cache"
echo ""

# 1. Wipe jdtls workspace cache
if [ -d "$jdtls_cache" ]; then
    rm -rf "$jdtls_cache"
    echo "[x] Removed jdtls cache: $jdtls_cache"
else
    echo "[ ] No jdtls cache found"
fi

# 2. Clean Eclipse/jdtls files and target from root + all modules
count=0
while IFS= read -r -d '' pom_dir; do
    dir="$(dirname "$pom_dir")"
    for f in .classpath .project .settings .factorypath; do
        if [ -e "$dir/$f" ]; then
            rm -rf "$dir/$f"
            echo "[x] Removed $dir/$f"
            count=$((count + 1))
        fi
    done
    if [ -d "$dir/target" ]; then
        rm -rf "$dir/target"
        echo "[x] Removed $dir/target"
        count=$((count + 1))
    fi
done < <(find "$root" -maxdepth 2 -name "pom.xml" -print0)

# also handle root-level files if no pom.xml at root (Gradle-only projects)
for f in .classpath .project .settings .factorypath; do
    if [ -e "$root/$f" ]; then
        rm -rf "$root/$f"
        echo "[x] Removed $root/$f"
        count=$((count + 1))
    fi
done

echo ""
echo "Done. Cleaned $count artifacts. Open nvim to reimport."
