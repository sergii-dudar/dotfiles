#!/usr/bin/env bash

# OLD_DIR="/home/serhii/tools/java-test-projs/Employee-Management-Sys/EmployeeManagementSystem/src/main/java/ua/dsm/corp/EmployeeManagementSystem/service"
# OLD_PACKAGE="ua.dsm.corp.EmployeeManagementSystem.service"
# NEW_PACKAGE="ua.dsm.corp.EmployeeManagementSystem.service.impl"
#
# NEW_FILE_PATH="/home/serhii/tools/java-test-projs/Employee-Management-Sys/EmployeeManagementSystem/src/main/java/ua/dsm/corp/EmployeeManagementSystem/service/impl/ServiceEmployeeUser.java"
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
SIBLING_TYPES=$7  # Optional: comma-separated list of sibling type names also being moved

# Validate inputs
if [[ -z "$OLD_DIR" || -z "$OLD_PACKAGE" || -z "$NEW_PACKAGE" || -z "$NEW_FILE_PATH" || -z "$OLD_TYPE_NAME" || -z "$NEW_TYPE_NAME" ]]; then
    echo "Error: Missing required arguments" >&2
    echo "Usage: $0 OLD_DIR OLD_PACKAGE NEW_PACKAGE NEW_FILE_PATH OLD_TYPE_NAME NEW_TYPE_NAME [SIBLING_TYPES]" >&2
    exit 1
fi

# Convert comma-separated sibling list to array
IFS=',' read -ra SIBLINGS_ARRAY <<< "$SIBLING_TYPES"

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

    # Get the package of the file being fixed
    file_package=$(rg -m1 '^package ' "$NEW_FILE_PATH" | sed 's/package \(.*\);/\1/' | xargs)

    # Helper function to check if a type is in the siblings list
    is_sibling() {
        local type=$1
        for sibling in "${SIBLINGS_ARRAY[@]}"; do
            if [[ "$sibling" == "$type" ]]; then
                return 0
            fi
        done
        return 1
    }

    # Note: We append all imports at the same line number (after last import)
    # Each append automatically shifts subsequent lines, so they stack correctly
    while read -r filename; do
        # echo "$filename"
        if rg -q "(^|[[:space:],;(}<])${filename}($|[[:space:],;(}\\.>])" "$NEW_FILE_PATH"; then
            # Determine the correct package for import
            local import_package="$OLD_PACKAGE"

            # If this file is a sibling (also being moved), import from NEW_PACKAGE instead
            if is_sibling "$filename"; then
                import_package="$NEW_PACKAGE"
            fi

            # Only add import if not in the same package
            if [[ "$file_package" != "$import_package" ]]; then
                import_line="import ${import_package}.${filename};"
                # Use append command with proper syntax for each OS
                if [[ "$(uname)" == "Darwin" ]]; then
                    # BSD sed requires literal newline in the command
                    sed -i '' "${LAST_IMPORT_LINE}a\\
${import_line}
" "$NEW_FILE_PATH"
                else
                    # GNU sed can use escape sequence
                    sed -i "${LAST_IMPORT_LINE}a\\${import_line}" "$NEW_FILE_PATH"
                fi
            fi
        fi
    done < <(fd --color=never -e java --max-depth 1 . "$OLD_DIR" -x basename {} .java)

    # import moved java file to java files in old package in case using
    fd --color=never -e java --max-depth 1 . "$OLD_DIR" | while read -r filename; do
        # echo "$filename"
        # Call sibling usage fix for each file in old directory
        "$HOME"/dotfiles/work/java/remane/fix-java-sibling-usage.sh "$filename" "$NEW_PACKAGE" "$OLD_TYPE_NAME" "$NEW_TYPE_NAME"
    done
fi