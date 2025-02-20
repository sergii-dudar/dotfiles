#!/bin/bash

EXEC_DIR="$1"
CACHE_FILE_NAME=".classpath.cache"
POM_FILE="pom.xml"

if [ ! -f "$CACHE_FILE_NAME" ] || [ "$POM_FILE" -nt "$CACHE_FILE_NAME" ]; then
    if [ -f $CACHE_FILE_NAME ]; then
        rm $CACHE_FILE_NAME
    fi
    mvn -q dependency:build-classpath -Dmdep.outputFile="$CACHE_FILE_NAME" -DincludeScope=runtime
fi


cache_file_dir="$EXEC_DIR"
while [ "$cache_file_dir" != "$HOME" ]; do
    found_file=$(fd --no-ignore -d 1 -t f -H "$CACHE_FILE_NAME" "$cache_file_dir")
    if [[ -n $found_file ]]; then
        break
    fi
    cache_file_dir=$(dirname "$cache_file_dir")
done

classpath=$(cat "$cache_file_dir"/$CACHE_FILE_NAME)
echo "$classpath:$cache_file_dir/target/classes"