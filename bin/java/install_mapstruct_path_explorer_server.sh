#!/usr/bin/env bash

ms_path_explorer_jar_name="mapstruct-path-explorer.jar"
ms_path_explorer_dir="$HOME/tools/java-extensions/mapstruct"
ms_path_explorer_jar="$ms_path_explorer_dir/mapstruct-path-explorer.jar"
ms_path_explorer_repo="$ms_path_explorer_dir/mapstruct-path-explorer-repo"
if [ -f "$ms_path_explorer_jar" ]; then
    echo "$ms_path_explorer_jar already exists, updating..."
    cd "$ms_path_explorer_repo" && git pull
else
    mkdir -p "$ms_path_explorer_dir" && cd "$ms_path_explorer_dir"
    git clone https://github.com/sergii-dudar/mapstruct-path-explorer.git "$ms_path_explorer_repo"
fi

cd "$ms_path_explorer_repo" \
    && ./mvnw clean package -U -DskipTests \
    && cd ./target \
    && mkdir -p "$ms_path_explorer_dir" \
    && mv "$ms_path_explorer_jar_name" "$ms_path_explorer_dir/"

echo "$ms_path_explorer_jar_name successfully installed to $ms_path_explorer_dir"