#!/usr/bin/env bash

#fd . "src/main/java/com/example/EmployeeManagementSystem/service" --max-depth 1 -e java | xargs rg --color=never -l ""
#ROOT="."

# ROOT="$HOME/tools/java-test-projs/Employee-Management-Sys/EmployeeManagementSystem"
# # echo "$ROOT"
# SEARCH_PATH="$ROOT/src/main/java/com/example/EmployeeManagementSystem/service"

# ls "$SEARCH_PATH"
# fd --full-path "$SEARCH_PATH" --max-depth 1 -e java
# fd --color=never "$SEARCH_PATH" --max-depth 1
# xargs rg --color=never -l ""
# fd --full-path '/home/serhii/tools/java-test-projs/Employee-Management-Sys/EmployeeManagementSystem/src/main/java/com/example/EmployeeManagementSystem/service' | xargs echo
# fd . "src/main/java/com/example/EmployeeManagementSystem/service" --color=never --max-depth 1 -e java
# fd --color=never --max-depth 1 -e java -p "$SEARCH_PATH"

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