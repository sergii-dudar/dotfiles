#!/opt/homebrew/bin/bash

# TODO:

set -euo pipefail

CURRENT_WORKSPACE=$(aerospace list-workspaces --focused)

for app_name in "$@"; do
    echo "app name: $app_name"

    if aerospace list-windows --workspace "$CURRENT_WORKSPACE" --format "%{app-name}" | grep "$app_name"; then
        echo "app name matched: $app_name"

        app_window_id=$(aerospace list-windows --workspace "$CURRENT_WORKSPACE" --format "%{window-id}%{right-padding} | '%{app-name}'" |
            grep "$app_name" |
            cut -d ' ' -f1 |
        head -n1)
        aerospace move-node-to-workspace NSP --window-id "$app_window_id"

    fi
done