#!/usr/bin/env bash

EXEC_DIR="$1"
EXEC_SCOPE="$2"
CACHE_FILE_NAME=".classpath.${EXEC_SCOPE}.cache"
POM_FILE="pom.xml"

cd "$EXEC_DIR" || exit

if [ ! -f "$CACHE_FILE_NAME" ] || [ "$POM_FILE" -nt "$CACHE_FILE_NAME" ]; then
    if [ -f "$CACHE_FILE_NAME" ]; then
        rm "$CACHE_FILE_NAME"
    fi
    # cache file to app root (where pom.xml file), mvn work from any child sub dir of project
    mvn -q dependency:build-classpath -Dmdep.outputFile="$CACHE_FILE_NAME" -DincludeScope="$EXEC_SCOPE"
fi

cache_file_dir="$EXEC_DIR"
while [ "$cache_file_dir" != "$HOME" ]; do
    found_file=$(fd --no-ignore -d 1 -t f -H "$CACHE_FILE_NAME" "$cache_file_dir")
    if [[ -n $found_file ]]; then
        break
    fi
    cache_file_dir=$(dirname "$cache_file_dir")
done

# classpath=$(cat "$cache_file_dir"/$CACHE_FILE_NAME)
classpath=$(bat -pp "$cache_file_dir"/"$CACHE_FILE_NAME")

# build sources
if [ "$EXEC_SCOPE" == "test" ]; then
    bash -c "mvn -q compile test-compile"
    echo "$classpath:$cache_file_dir/target/classes:$cache_file_dir/target/test-classes:$cache_file_dir/target/generated-sources:$cache_file_dir/target/generated-test-sources"
else
    bash -c "mvn -q compile"
    echo "$classpath:$cache_file_dir/target/classes:$cache_file_dir/target/generated-sources"
fi
