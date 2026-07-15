#!/usr/bin/env bash

services=$(command ls "$HOME/serhii.home/work/git.infra/ua-avalaunch-domain-provisioning/payments/payments/services")

skip_projects=(
    # Add either service folder names or full repo names here.
    # example-service
    # ua-payments-example-service
    ua-payments-prometheus-clickhouse-source-connector
    ua-payments-payment-validation-processor
    ua-payments-payment-flink-station
    ua-payments-payment-flink-rubicon
    ua-payments-payment-flink-hub
    ua-payments-payment-engine-rail
    ua-payments-payment-api-gateway
    ua-payments-form-payment-management
    ua-payments-form-payment-avto-syto
    ua-payments-flink-jobs
    ua-payments-flink-statefun
    ua-payments-external-dependencies
    ua-payments-clickhouse-kafka
)

should_skip_project() {
    local folder="$1"
    local repo_dir="$2"
    local skipped_project

    for skipped_project in "${skip_projects[@]}"; do
        if [[ "$skipped_project" == "$folder" || "$skipped_project" == "$repo_dir" ]]; then
            return 0
        fi
    done

    return 1
}

for folder in $services; do
    repo_dir="ua-payments-${folder}"
    repo="git@code.rbi.tech:RBUA/${repo_dir}.git"

    if should_skip_project "$folder" "$repo_dir"; then
        echo "Skipping configured project: $repo_dir"
        continue
    fi

    if [ -e "$repo_dir" ]; then
        echo "Skipping existing: $repo_dir"
        continue
    fi

    echo "Cloning repo: $repo"
    git clone "$repo"
done