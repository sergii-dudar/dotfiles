#!/bin/sh
# Runs one refactor scenario headlessly and prints the resulting tree, package/import lines and javac status.
#
#   run.sh <fixture-dir> <src> <dst> [<src> <dst> ...]
#
# The moves themselves are performed by driver.lua (like a file manager would), then the module fixes the
# project in test mode. Requires: nvim, rg, fd, gsed (macOS) / sed, javac.
set -u
P="$1"
shift
S=$(cd "$(dirname "$0")" && pwd)
# tests/ -> refactor -> java -> modules -> lua -> <nvim config dir>
CFG=$(cd "$S/../../../../.." && pwd)

(cd "$P" && nvim --headless -u NONE --cmd "set rtp+=$CFG" -l "$S/driver.lua" "$@" 2>&1)

echo "=== TREE ==="
(cd "$P" && find src -name '*.java' | sort)
echo "=== PACKAGE / IMPORT LINES ==="
(cd "$P" && grep -rn --include='*.java' -E '^(package|import) ' src | sort)

rm -rf "$P/out" && mkdir -p "$P/out"
echo "=== JAVAC MAIN ==="
find "$P/src/main" -name '*.java' >"$P/out/main.txt"
javac -d "$P/out" "@$P/out/main.txt" && echo "main OK"
echo "=== JAVAC TEST ==="
find "$P/src/test" -name '*.java' >"$P/out/test.txt"
javac -d "$P/out" -cp "$P/out" "@$P/out/test.txt" && echo "test OK"
