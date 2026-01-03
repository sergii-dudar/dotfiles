#!/usr/bin/env bash

# OLD_DIR="/home/serhii/tools/java-test-projs/Employee-Management-Sys/EmployeeManagementSystem/src/main/java/com/example/EmployeeManagementSystem/service"
# OLD_PACKAGE="com.example.EmployeeManagementSystem.service"
# NEW_FILE_PATH="/home/serhii/tools/java-test-projs/Employee-Management-Sys/EmployeeManagementSystem/src/main/java/com/example/EmployeeManagementSystem/service/impl/ServiceEmployeeUser.java"

# OLD_DIR="/home/serhii/tools/java-test-projs/Employee-Management-Sys/EmployeeManagementSystem/src/main/java/com/example/EmployeeManagementSystem/service"
# OLD_PACKAGE="com.example.EmployeeManagementSystem.service"
# NEW_FILE_PATH="/home/serhii/tools/java-test-projs/Employee-Management-Sys/EmployeeManagementSystem/src/main/java/com/example/EmployeeManagementSystem/service/impl/MarkerInterface.java"

# OLD_TYPE_NAME="MarkerInterface"
# NEW_TYPE_NAME="MarkerInterface"
# NEW_PACKAGE="com.example.EmployeeManagementSystem.service.impl"

OLD_DIR=$1
OLD_PACKAGE=$2
NEW_PACKAGE=$3
NEW_FILE_PATH=$4 #$3
OLD_TYPE_NAME=$5
NEW_TYPE_NAME=$6

# import uding types in moved java file from old package
LAST_IMPORT_LINE=$(rg -n '^import ' "$NEW_FILE_PATH" | tail -n 1 | cut -d: -f1)
LAST_IMPORT_LINE=${LAST_IMPORT_LINE:-2}

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

# import moved java file to java files in old package in case using
fd --color=never -e java --max-depth 1 . "$OLD_DIR" | while read -r filename; do
    # echo "$filename"
    if rg -q "(^|[[:space:],;(}<])${OLD_TYPE_NAME}($|[[:space:],;(}\\.>])" "$filename"; then
        last_imported_num=$(rg -n '^import ' "$filename" | tail -n 1 | cut -d: -f1)
        last_imported_num=2
        # echo "$filename"
        # echo "$last_imported_num"
        ((last_imported_num++))
        import_line="import ${NEW_PACKAGE}.${NEW_TYPE_NAME};"
        sed -i "${last_imported_num}i\\${import_line}" "$filename"
        sed -i -E "s/([[:space:],;(}<])${OLD_TYPE_NAME}([[:space:],;(}\.>])/\1${NEW_TYPE_NAME}\2/g" "$filename"
    fi
done