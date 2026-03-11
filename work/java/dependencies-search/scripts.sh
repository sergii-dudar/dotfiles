#!/bin/bash
# extract-jars.sh
SEARCH_DIR="$HOME/.jar-search-cache"
mkdir -p "$SEARCH_DIR"

# For Maven — scope to your project's deps only
# find ~/.m2/repository -name "*.jar" ! -name "*sources*" ! -name "*javadoc*" | while read jar; do
#     dest="$SEARCH_DIR/$(basename $jar .jar)"
#     mkdir -p "$dest"
#     unzip -qo "$jar" -d "$dest" 2>/dev/null
# done

###

find ~/.m2/repository -name "*sources.jar" | while read jar; do
    dest="$SEARCH_DIR/$(basename $jar .jar)"
    mkdir -p "$dest"
    unzip -qo "$jar" -d "$dest" 2>/dev/null
done

###

# Put all paths in a file
find ~/.m2 -name "*.jar" -exec unzip -d /tmp/jar-cache/{} {} \;

# Or just list dirs
find ~/.m2 -mindepth 3 -maxdepth 3 -type d > /tmp/dirs.txt

# Then feed to rg via xargs
cat /tmp/dirs.txt | xargs rg "search text"

###

grep '*sources*' /tmp/dirs.txt | xargs rg "search text" -g "*.java" -g "*.yaml"
find ~/.m2 -type d -name "*sources*" | xargs rg "search text" -g "*.java" -g "*.yaml"

fd -t d -g '*sources*' ~/.m2 | xargs rg "search text" -g "*.java" -g "*.yaml"
fd -t d -g '*sources*' ~/.m2 -0 | xargs -0 rg "search text" -g "*.java" -g "*.yaml"

d --color=never -t d -g '*sources*' . | xargs rg "CollectionUtil" -g "*.java" -g "*.yaml"
