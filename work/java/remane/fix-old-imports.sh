#!/usr/bin/env bash

# OLD_DIR="/home/serhii/tools/java-test-projs/Employee-Management-Sys/EmployeeManagementSystem/src/main/java/com/example/EmployeeManagementSystem/service"
# OLD_PACKAGE="com.example.EmployeeManagementSystem.service"
# NEW_PACKAGE="com.example.EmployeeManagementSystem.service.impl"
#
# NEW_FILE_PATH="/home/serhii/tools/java-test-projs/Employee-Management-Sys/EmployeeManagementSystem/src/main/java/com/example/EmployeeManagementSystem/service/impl/ServiceEmployeeUser.java"
#
# OLD_TYPE_NAME="MarkerInterface"
# NEW_TYPE_NAME="MarkerInterfaceUser"

# Detect OS and set appropriate sed -i flag
# macOS (BSD sed) requires -i '' while Linux (GNU sed) uses -i
if [[ "$(uname)" == "Darwin" ]]; then
    SED_INPLACE="sed -i ''"
else
    SED_INPLACE="sed -i"
fi

OLD_DIR=$1
OLD_PACKAGE=$2
NEW_PACKAGE=$3
NEW_FILE_PATH=$4
OLD_TYPE_NAME=$5
NEW_TYPE_NAME=$6

# Validate inputs
if [[ -z "$OLD_DIR" || -z "$OLD_PACKAGE" || -z "$NEW_PACKAGE" || -z "$NEW_FILE_PATH" || -z "$OLD_TYPE_NAME" || -z "$NEW_TYPE_NAME" ]]; then
    echo "Error: Missing required arguments" >&2
    echo "Usage: $0 OLD_DIR OLD_PACKAGE NEW_PACKAGE NEW_FILE_PATH OLD_TYPE_NAME NEW_TYPE_NAME" >&2
    exit 1
fi

if [[ ! -f "$NEW_FILE_PATH" ]]; then
    echo "Error: File not found: $NEW_FILE_PATH" >&2
    exit 1
fi

# OLD_DIR might not exist if all files were moved from it - this is OK
# We only process files in OLD_DIR if the directory still exists

# import uding types in moved java file from old package
# Only process if old directory still exists (some files remained there)
if [[ -d "$OLD_DIR" ]]; then
    LAST_IMPORT_LINE=$(rg -n '^import ' "$NEW_FILE_PATH" | tail -n 1 | cut -d: -f1)
    LAST_IMPORT_LINE=${LAST_IMPORT_LINE:-2}

    fd --color=never -e java --max-depth 1 . "$OLD_DIR" \
        -x basename {} .java | while read -r filename; do
        # echo "$filename"
        if rg -q "(^|[[:space:],;(}<])${filename}($|[[:space:],;(}\\.>])" "$NEW_FILE_PATH"; then
            # echo "$filename"
            ((LAST_IMPORT_LINE++))
            import_line="import ${OLD_PACKAGE}.${filename};"
            $SED_INPLACE "${LAST_IMPORT_LINE}i\\${import_line}" "$NEW_FILE_PATH"
        fi
    done

    # import moved java file to java files in old package in case using
    fd --color=never -e java --max-depth 1 . "$OLD_DIR" | while read -r filename; do
        # echo "$filename"
        # Call sibling usage fix for each file in old directory
        "$HOME"/dotfiles/work/java/remane/fix-java-sibling-usage.sh "$filename" "$NEW_PACKAGE" "$OLD_TYPE_NAME" "$NEW_TYPE_NAME"
    done
fi