#!/usr/bin/env bash

set -euo pipefail

CURRENT_WORKSPACE=$(aerospace list-workspaces --focused)

ALL_WS_APP_NAMES=$(aerospace list-windows --workspace "$CURRENT_WORKSPACE" --format "%{app-name}")
ALL_WS_APP_IDS=$(aerospace list-windows --workspace "$CURRENT_WORKSPACE" --format "%{window-id}%{right-padding} | '%{app-name}'")

for app_name in "$@"; do
    # echo "app name: $app_name"

    if echo "$ALL_WS_APP_NAMES" | grep "$app_name"; then
        # echo "app name matched: $app_name"
        app_window_id=$(echo "$ALL_WS_APP_IDS" | grep "$app_name" | cut -d ' ' -f1 | head -n1)
        aerospace move-node-to-workspace NSP --window-id "$app_window_id"
    fi
done

sketchybar --trigger scratchpad_update