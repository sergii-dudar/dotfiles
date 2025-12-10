#!/usr/bin/env bash

install_dir="$HOME/tools/java-extensions/junit"
jar_name="junit-platform-console-standalone.jar"
jar_path="$install_dir/$jar_name"
if [ -f "$jar_path" ]; then
    echo "$jar_path already exists"
    exit 0
fi

mkdir -p "$install_dir" && cd "$install_dir"
wget -O "$jar_name" "https://repo1.maven.org/maven2/org/junit/platform/junit-platform-console-standalone/6.0.1/junit-platform-console-standalone-6.0.1.jar"
echo "$jar_name successfully downloaded to $jar_path"
