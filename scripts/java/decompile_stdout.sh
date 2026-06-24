#!/usr/bin/env bash
# Decompile a single .class file with Fernflower and print the resulting Java
# source to stdout, syntax-highlighted with `bat`. Intended for terminal-mode
# editor previewers that render ANSI (e.g. Snacks' cmd previewer without an
# explicit filetype). `--color=always` forces colors even though stdout is a
# pipe rather than a tty.
set -euo pipefail

CLASSFILE="${1:-}"
FERNFLOWER="$HOME/tools/java-extensions/decompiler/fernflower.jar"

if [[ -z "$CLASSFILE" || ! -f "$CLASSFILE" ]]; then
    echo "// file not found: $CLASSFILE" >&2
    exit 1
fi
if [[ ! -f "$FERNFLOWER" ]]; then
    echo "// fernflower not found: $FERNFLOWER" >&2
    exit 1
fi

# Isolated temp dir per invocation avoids stale/colliding outputs.
OUTDIR="$(mktemp -d)"
trap 'rm -rf "$OUTDIR"' EXIT

java -jar "$FERNFLOWER" "$CLASSFILE" "$OUTDIR" >/dev/null 2>&1

JAVA_FILE="$OUTDIR/$(basename "${CLASSFILE%.class}.java")"
if [[ -f "$JAVA_FILE" ]]; then
    bat --language=java --color=always --paging=never --style=numbers "$JAVA_FILE"
else
    echo "// Decompilation failed for: $CLASSFILE" >&2
    exit 1
fi
