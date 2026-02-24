#!/usr/bin/env bash

install_dir="$HOME/tools/java-extensions/jmockit"
jmockit_jar_name="jmockit.jar"
jmockit_jar_path="$install_dir/$jmockit_jar_name"
if [ -f "$jmockit_jar_path" ]; then
    echo "$jmockit_jar_path already exists, reinstalling"
    rm "$jmockit_jar_path"
fi

mkdir -p "$install_dir" && cd "$install_dir"

# Define the major version to track
major="1"

# Fetch all versions and find the latest minor version for the specified major
latest_version=$(curl -s "https://repo1.maven.org/maven2/org/jmockit/jmockit/maven-metadata.xml" | grep '<version>' | sed 's/.*<version>\(.*\)<\/version>.*/\1/' | grep "^${major}\." | sort -V | tail -n1)

if [ -z "$latest_version" ]; then
    echo "Failed to fetch latest minor version for major $major"
    exit 1
fi

echo "Downloading jmockit version $latest_version"
wget -O "$jmockit_jar_name" "https://repo1.maven.org/maven2/org/jmockit/jmockit/$latest_version/jmockit-$latest_version.jar"
echo "$jmockit_jar_name successfully downloaded to $jmockit_jar_path"