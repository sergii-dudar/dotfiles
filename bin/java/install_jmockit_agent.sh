#!/usr/bin/env bash

install_dir="$HOME/tools/java-extensions/jmockit"
jmockit_jar_name="jmockit.jar"
jmockit_jar_path="$install_dir/$jmockit_jar_name"
if [ -f "$jmockit_jar_path" ]; then
    echo "$jmockit_jar_path already exists, reinstalling"
    rm "$jmockit_jar_path"
fi

mkdir -p "$install_dir" && cd "$install_dir"
wget -O "$jmockit_jar_name" "https://repo1.maven.org/maven2/org/jmockit/jmockit/1.50/jmockit-1.50.jar"
echo "$jmockit_jar_name successfully downloaded to $jmockit_jar_path"