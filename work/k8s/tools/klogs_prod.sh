#!/usr/bin/env bash

namespace=payments-prod
context=aws-avalaunch-prod

mapfile -t deployments < <(kubectl get deployments -n "$namespace" --context "$context" -o jsonpath='{.items[*].metadata.name}' \
    | tr ' ' '\n' | fzf --multi --header="TAB=select  ENTER=confirm  ESC=cancel")
[[ ${#deployments[@]} -eq 0 ]] && { echo "No deployments selected"; exit 1; }
./klogs-multi.sh "${deployments[@]}" -n "$namespace" --context "$context" --clean -p -f