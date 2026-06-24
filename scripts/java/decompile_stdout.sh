#!/usr/bin/env bash
# Decompile a single .class file with Fernflower and print the resulting Java
# source to stdout, syntax-highlighted with `bat`. Intended for terminal-mode
# editor previewers that render ANSI (e.g. Snacks' cmd previewer without an
# explicit filetype). `--color=always` forces colors even though stdout is a
# pipe rather than a tty.
set -euo pipefail

CLASSFILE="${1:-}"
FERNFLOWER="$HOME/tools/java-extensions/decompiler/fernflower.jar"

# bat is installed as `batcat` on Debian/Ubuntu (name clash) but `bat` on Arch
# and macOS/Homebrew. Detect whichever exists; fall back to `cat` (no colors)
# if bat isn't installed at all.
if command -v bat >/dev/null 2>&1; then
    BAT_BIN="bat"
elif command -v batcat >/dev/null 2>&1; then
    BAT_BIN="batcat"
else
    BAT_BIN=""
fi

if [[ -z "$CLASSFILE" || ! -f "$CLASSFILE" ]]; then
    echo "// file not found: $CLASSFILE" >&2
    exit 1
fi
if [[ ! -f "$FERNFLOWER" ]]; then
    echo "// fernflower not found: $FERNFLOWER" >&2
    exit 1
fi

# Isolated temp dir per invocation avoids stale/colliding outputs. Clean it up
# on normal exit AND on the signals a previewer sends when killing the job
# (Snacks/Neovim send TERM when you move to the next item before we finish), so
# interrupted decompiles don't leak temp dirs. SIGKILL can't be trapped, but the
# OS reaps $TMPDIR periodically.
OUTDIR="$(mktemp -d)"
trap 'rm -rf "$OUTDIR"' EXIT
trap 'rm -rf "$OUTDIR"; exit 143' INT TERM HUP

java -jar "$FERNFLOWER" "$CLASSFILE" "$OUTDIR" >/dev/null 2>&1

JAVA_FILE="$OUTDIR/$(basename "${CLASSFILE%.class}.java")"
if [[ -f "$JAVA_FILE" ]]; then
    if [[ -n "$BAT_BIN" ]]; then
        "$BAT_BIN" --language=java --color=always --paging=never --style=numbers "$JAVA_FILE"
    else
        cat "$JAVA_FILE"
    fi
else
    echo "// Decompilation failed for: $CLASSFILE" >&2
    exit 1
fi
