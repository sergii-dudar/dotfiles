#!/usr/bin/env bash

CLASSFILE="$1"
# OUTDIR="$(mktemp -d)"
OUTDIR="${TMPDIR:-/tmp}"

JAVA_FILE_NAME="$(basename "${CLASSFILE%.class}.java")"
DECOMPILED_JAVA="${OUTDIR}${JAVA_FILE_NAME}"
java -jar "$HOME/tools/java-extensions/decompiler/fernflower.jar" "$CLASSFILE" "$OUTDIR" >/dev/null 2>&1

# echo "$DECOMPILED_JAVA"

if [[ -f "$DECOMPILED_JAVA" ]]; then
    bat --plain "$DECOMPILED_JAVA" && rm "$DECOMPILED_JAVA"
else
    echo "Decompilation failed."
fi