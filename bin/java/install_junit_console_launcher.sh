#!/usr/bin/env bash

install_dir="$HOME/tools/java-extensions/junit"
jar_name="junit-platform-console-standalone.jar"
jar_path="$install_dir/$jar_name"
if [ -f "$jar_path" ]; then
    echo "$jar_path already exists, reinstalling"
    rm "$jar_path"
fi

mkdir -p "$install_dir" && cd "$install_dir"

# Define the major.minor version to track
major_minor="6.0"

# Fetch all versions and find the latest patch version for the specified major.minor
# latest_version=$(curl -s "https://repo1.maven.org/maven2/org/junit/platform/junit-platform-console-standalone/maven-metadata.xml" | grep '<latest>' | sed 's/.*<latest>\(.*\)<\/latest>.*/\1/')
latest_version=$(curl -s "https://repo1.maven.org/maven2/org/junit/platform/junit-platform-console-standalone/maven-metadata.xml" | grep '<version>' | sed 's/.*<version>\(.*\)<\/version>.*/\1/' | grep "^${major_minor}\." | sort -V | tail -n1)

if [ -z "$latest_version" ]; then
    echo "Failed to fetch latest patch version for $major_minor"
    exit 1
fi

echo "Downloading JUnit Console Launcher version $latest_version"
wget -O "$jar_name" "https://repo1.maven.org/maven2/org/junit/platform/junit-platform-console-standalone/$latest_version/junit-platform-console-standalone-$latest_version.jar"
echo "$jar_name successfully downloaded to $jar_path"