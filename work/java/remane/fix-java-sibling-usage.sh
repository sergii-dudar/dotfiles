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

FILE_PATH_TO_APPLY_FIX=$1
NEW_PACKAGE=$2
OLD_TYPE_NAME=$3
NEW_TYPE_NAME=$4
FILE_DST_PACKAGE=$5

# Validate inputs
if [[ -z "$FILE_PATH_TO_APPLY_FIX" || -z "$NEW_PACKAGE" || -z "$OLD_TYPE_NAME" || -z "$NEW_TYPE_NAME" ]]; then
    echo "Error: Missing required arguments" >&2
    echo "Usage: $0 FILE_PATH NEW_PACKAGE OLD_TYPE_NAME NEW_TYPE_NAME [FILE_DST_PACKAGE]" >&2
    exit 1
fi

if [[ ! -f "$FILE_PATH_TO_APPLY_FIX" ]]; then
    echo "Error: File not found: $FILE_PATH_TO_APPLY_FIX" >&2
    exit 1
fi

# import moved java file to java files in old package in case using
if rg -q "(^|[[:space:],;(}<])${OLD_TYPE_NAME}($|[[:space:],;(}\\.>])" "$FILE_PATH_TO_APPLY_FIX"; then
    # Determine the file's destination package
    # Use FILE_DST_PACKAGE if provided, otherwise read from file's current package declaration
    if [[ -n "$FILE_DST_PACKAGE" ]]; then
        file_package="$FILE_DST_PACKAGE"
    else
        file_package=$(rg -m1 '^package ' "$FILE_PATH_TO_APPLY_FIX" | sed 's/package \(.*\);/\1/' | xargs)
    fi

    if [[ "$file_package" != "$NEW_PACKAGE" ]]; then
        # Only add import if not in the same package
        last_imported_num=$(rg -n '^import ' "$FILE_PATH_TO_APPLY_FIX" | tail -n 1 | cut -d: -f1)
        last_imported_num=${last_imported_num:-2}

        import_line="import ${NEW_PACKAGE}.${NEW_TYPE_NAME};"
        # Use append command with proper syntax for each OS
        # Note: Append after the last import line (multiple appends will stack correctly)
        if [[ "$(uname)" == "Darwin" ]]; then
            # BSD sed requires literal newline in the command
            sed -i '' "${last_imported_num}a\\
${import_line}
" "$FILE_PATH_TO_APPLY_FIX"
        else
            # GNU sed can use escape sequence
            sed -i "${last_imported_num}a\\${import_line}" "$FILE_PATH_TO_APPLY_FIX"
        fi
    fi

    # Always update type references if the name changed
    if [[ "$OLD_TYPE_NAME" != "$NEW_TYPE_NAME" ]]; then
        $SED_INPLACE -E "s/([[:space:],;(}<])${OLD_TYPE_NAME}([[:space:],;(}\.>])/\1${NEW_TYPE_NAME}\2/g" "$FILE_PATH_TO_APPLY_FIX"
    fi
fi