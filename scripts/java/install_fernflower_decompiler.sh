#!/usr/bin/env bash

fernflower_dir="$HOME/tools/java-extensions/decompiler"
fernflower_jar="$fernflower_dir/fernflower.jar"
fernflower_repo="$fernflower_dir/fernflower-repo"
if [ -f "$fernflower_jar" ]; then
    echo "$fernflower_jar already exists, updating"
    cd "$fernflower_repo" && git pull
else
    mkdir -p "$fernflower_dir" && cd "$fernflower_dir"
    git clone https://github.com/JetBrains/fernflower.git fernflower-repo
fi

cd "$fernflower_repo" \
    && ./gradlew :installDist \
    && cd ./build/libs \
    && mkdir -p "$fernflower_dir" \
    && mv fernflower.jar "$fernflower_dir/"

echo "fernflower decompiler successfully installed to $fernflower_dir"