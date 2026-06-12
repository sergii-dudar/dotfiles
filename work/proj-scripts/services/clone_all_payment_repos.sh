#!/usr/bin/env bash

services=$(command ls "$HOME/serhii.home/work/git.infra/ua-avalaunch-domain-provisioning/payments/payments/services")

for folder in $services; do
    repo_dir="ua-payments-${folder}"
    repo="git@code.rbi.tech:RBUA/${repo_dir}.git"

    if [ -e "$repo_dir" ]; then
        echo "Skipping existing: $repo_dir"
        continue
    fi

    echo "Cloning repo: $repo"
    git clone "$repo"
done
