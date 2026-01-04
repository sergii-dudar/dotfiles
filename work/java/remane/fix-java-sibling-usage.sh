#!/usr/bin/env bash

# OLD_DIR="/home/serhii/tools/java-test-projs/Employee-Management-Sys/EmployeeManagementSystem/src/main/java/com/example/EmployeeManagementSystem/service"
# OLD_PACKAGE="com.example.EmployeeManagementSystem.service"
# NEW_PACKAGE="com.example.EmployeeManagementSystem.service.impl"
#
# NEW_FILE_PATH="/home/serhii/tools/java-test-projs/Employee-Management-Sys/EmployeeManagementSystem/src/main/java/com/example/EmployeeManagementSystem/service/impl/ServiceEmployeeUser.java"
#
# OLD_TYPE_NAME="MarkerInterface"
# NEW_TYPE_NAME="MarkerInterfaceUser"

FILE_PATH_TO_APPLY_FIX=$1
NEW_PACKAGE=$2
OLD_TYPE_NAME=$3
NEW_TYPE_NAME=$4

# import moved java file to java files in old package in case using
if rg -q "(^|[[:space:],;(}<])${OLD_TYPE_NAME}($|[[:space:],;(}\\.>])" "$FILE_PATH_TO_APPLY_FIX"; then
    last_imported_num=$(rg -n '^import ' "$FILE_PATH_TO_APPLY_FIX" | tail -n 1 | cut -d: -f1)
    last_imported_num=${last_imported_num:-2}
    # echo "$filename"
    # echo "$last_imported_num"
    ((last_imported_num++))
    import_line="import ${NEW_PACKAGE}.${NEW_TYPE_NAME};"
    sed -i "${last_imported_num}i\\${import_line}" "$FILE_PATH_TO_APPLY_FIX"
    sed -i -E "s/([[:space:],;(}<])${OLD_TYPE_NAME}([[:space:],;(}\.>])/\1${NEW_TYPE_NAME}\2/g" "$FILE_PATH_TO_APPLY_FIX"
fi