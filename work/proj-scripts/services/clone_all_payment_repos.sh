#!/usr/bin/env bash

services=$(command ls "$HOME/serhii.home/work/git.infra/ua-avalaunch-domain-provisioning/payments/payments/services")

for folder in $services; do
    # echo "service: ""$folder"
    repo="git@code.rbi.tech:RBUA/ua-payments-${folder}.git"
    echo "Clonning repo: $repo"
    git clone $repo
done

