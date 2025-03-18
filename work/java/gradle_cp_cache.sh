#!/usr/bin/env bash

# ./gradlew -q classes && java -cp "$(./gradlew -q printClasspath)" ua.serhii.application.Main
EXEC_DIR="$1"
CACHE_FILE_NAME=".classpath.cache"
POM_FILE="build.gradle"

cd "$EXEC_DIR" || exit

if [ ! -f "$CACHE_FILE_NAME" ] || [ "$POM_FILE" -nt "$CACHE_FILE_NAME" ]; then
    if [ -f $CACHE_FILE_NAME ]; then
        rm $CACHE_FILE_NAME
    fi
    ./gradlew -q printClasspath > $CACHE_FILE_NAME
fi

cache_file_dir="$EXEC_DIR"
while [ "$cache_file_dir" != "$HOME" ]; do
    found_file=$(fd --no-ignore -d 1 -t f -H "$CACHE_FILE_NAME" "$cache_file_dir")
    if [[ -n $found_file ]]; then
        break
    fi
    cache_file_dir=$(dirname "$cache_file_dir")
done

# build sources
bash -c "./gradlew -q classes"

# classpath=$(cat "$cache_file_dir"/$CACHE_FILE_NAME)
classpath=$(bat -pp "$cache_file_dir"/$CACHE_FILE_NAME)
echo "$classpath"