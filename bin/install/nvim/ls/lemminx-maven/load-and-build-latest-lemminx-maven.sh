#!/usr/bin/env bash

lemminx_dir="$HOME/tools/nvim-extensions/lemminx/"
lemminx_maven_dir="$lemminx_dir/lemminx-maven"
if [ -d "$lemminx_maven_dir" ]; then
    echo "$lemminx_maven_dir already exists"
    exit 0
fi

mkdir -p "$lemminx_dir" && cd "$lemminx_dir"
git clone https://github.com/eclipse-lemminx/lemminx-maven.git lemminx-maven-repo
cd "lemminx-maven-repo" \
    && ./mvnw -DskipTests package \
    && cd ./lemminx-maven/target \
    && unzip lemminx-maven-*-with-dependencies.zip -d lemminx-maven/ \
    && mv lemminx-maven "$lemminx_maven_dir"

echo "Lemminx Maven Extension successfully installed to $lemminx_ls_dir"

# # Fetch the latest release JSON from the GitHub API
# latest_json=$(curl -s https://api.github.com/repos/eclipse-lemminx/lemminx-maven/releases/latest)
#
# zip_url=$(echo "$latest_json" | jq -r '.zipball_url')
# version=$(echo "$latest_json" | jq -r '.tag_name')
# name=lemminx-maven-${version}
#
# wget -O "${name}.zip" "$zip_url"
# unzip "${name}.zip"
# cd "./eclipse-lemminx-lemminx-maven-"* \
    #     && ./mvnw -DskipTests package \
    #     && cd ./lemminx-maven/target \
    #     && unzip lemminx-maven-*-with-dependencies.zip -d lemminx-maven/ \
    #     && mv lemminx-maven "$lemminx_maven_dir"
