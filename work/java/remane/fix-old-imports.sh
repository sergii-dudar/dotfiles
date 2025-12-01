#!/usr/bin/env bash

# OLD_DIR="/home/serhii/tools/java-test-projs/Employee-Management-Sys/EmployeeManagementSystem/src/main/java/com/example/EmployeeManagementSystem/service"
# OLD_PACKAGE="com.example.EmployeeManagementSystem.service"
# NEW_FILE_PATH="/home/serhii/tools/java-test-projs/Employee-Management-Sys/EmployeeManagementSystem/src/main/java/com/example/EmployeeManagementSystem/service/impl/ServiceEmployeeUser.java"
OLD_DIR=$1
OLD_PACKAGE=$2
NEW_FILE_PATH=$3

LAST_IMPORT_LINE=$(rg -n '^import ' "$NEW_FILE_PATH" | tail -n 1 | cut -d: -f1)
fd --color=never -e java --max-depth 1 . "$OLD_DIR" \
    -x basename {} .java | while read -r filename; do
    # echo "$filename"
    if rg -q "(^|[[:space:],;(}<])${filename}($|[[:space:],;(}\\.>])" "$NEW_FILE_PATH"; then
        # echo "$filename"
        ((LAST_IMPORT_LINE++))
        import_line="import ${OLD_PACKAGE}.${filename};"
        sed -i "${LAST_IMPORT_LINE}i\\${import_line}" "$NEW_FILE_PATH"
    fi
done