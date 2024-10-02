#!/bin/bash

CACHE_FILE=".classpath.cache"
POM_FILE="pom.xml"

if [ ! -f "$CACHE_FILE" ] || [ "$POM_FILE" -nt "$CACHE_FILE" ]; then
    if [ -f $CACHE_FILE ]; then
        rm $CACHE_FILE
    fi
    mvn -q dependency:build-classpath -Dmdep.outputFile="$CACHE_FILE" -DincludeScope=runtime
fi

cat $CACHE_FILE
