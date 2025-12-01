#!/usr/bin/env bash

OLD_DIR="/home/serhii/tools/java-test-projs/Employee-Management-Sys/EmployeeManagementSystem/src/main/java/com/example/EmployeeManagementSystem/service"
OLD_PACKAGE="com.example.EmployeeManagementSystem.service"
NEW_FILE_PATH="/home/serhii/tools/java-test-projs/Employee-Management-Sys/EmployeeManagementSystem/src/main/java/com/example/EmployeeManagementSystem/service/impl/ServiceEmployeeUser.java"

last_import_line=$(rg -n '^import ' "$NEW_FILE_PATH" | tail -n 1 | cut -d: -f1)
fd --color=never -e java --max-depth 1 . "$OLD_DIR" \
    -x basename {} .java | while read -r filename; do
    # echo "$filename"
    if rg -q "(^|[[:space:],;(}<])${filename}($|[[:space:],;(}\\.>])" "$NEW_FILE_PATH"; then
        # echo "$filename"
        ((last_import_line++))
        import_line="import ${OLD_PACKAGE}.${filename};"
        sed -i "${last_import_line}i\\${import_line}" "$NEW_FILE_PATH"
    fi
done