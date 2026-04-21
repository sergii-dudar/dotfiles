#!/usr/bin/env bash

namespace=payments-dev
context=aws-avalaunch-dev

deployment_name=$(kubectl get deployments -n "$namespace" --context "$context" -o jsonpath='{.items[*].metadata.name}' \
    | tr ' ' '\n' | fzf)
./klogs.sh "$deployment_name" -n "$namespace" --context "$context" --clean -p -f