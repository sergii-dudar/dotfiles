#!/usr/bin/env bash

CLASSFILE="$1"
# OUTDIR="$(mktemp -d)"
OUTDIR="${TMPDIR}"

JAVA_FILE_NAME="$(basename "${CLASSFILE%.class}.java")"
DECOMPILED_JAVA="${OUTDIR}${JAVA_FILE_NAME}"
DECOMPILED_JAVA_SYNTAX="${OUTDIR}Syntax${JAVA_FILE_NAME}"
java -jar "$HOME/tools/java-extensions/decompiler/fernflower.jar" "$CLASSFILE" "$OUTDIR" >/dev/null 2>&1
bat --color=always --paging=never --plain --language=java "$DECOMPILED_JAVA" > "$DECOMPILED_JAVA_SYNTAX"
#echo "$DECOMPILED_JAVA_SYNTAX"
#cat "$DECOMPILED_JAVA_SYNTAX"