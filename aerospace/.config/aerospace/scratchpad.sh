#!/usr/bin/env bash

set -euo pipefail

APP_ID="$1"
APP_NAME="$2"
CURRENT_WORKSPACE=$(aerospace list-workspaces --focused)
CONFIG_DIR="$HOME/.config/aerospace/"

function get_window_id() {
    aerospace list-windows --all --format "%{window-id}%{right-padding} | '%{app-name}'" | \
        grep "$APP_NAME" | \
        cut -d' ' -f1 | \
        head -n1
}

function focus_app() {
    local app_window_id
    app_window_id=$(get_window_id)
    # echo "appid=$app_window_id"
    aerospace move-node-to-workspace "$CURRENT_WORKSPACE" --window-id "$app_window_id" ; \
        aerospace focus --window-id "$app_window_id"
}

function is_app_closed() {
    ! aerospace list-windows --all --format '%{app-name}' | grep -q "$APP_NAME"
}

function move_app_to_scratchpad() {
    local app_window_id
    app_window_id=$(aerospace list-windows --workspace "$CURRENT_WORKSPACE" --format "%{window-id}%{right-padding} | '%{app-name}'" |
        grep "$APP_NAME" |
        cut -d ' ' -f1 |
    head -n1)
    aerospace move-node-to-workspace NSP --window-id "$app_window_id"
}

function main() {
    if is_app_closed; then
        open -a "$APP_NAME"
        sleep 0.5
        "$CONFIG_DIR/size_and_center_float.sh" "$APP_NAME"
        #elif aerospace list-windows --workspace "$CURRENT_WORKSPACE" --format "%{app-bundle-id}" | grep -q "$APP_ID"; then
    elif aerospace list-windows --workspace "$CURRENT_WORKSPACE" --format "%{app-name}" | grep "$APP_NAME"; then
        move_app_to_scratchpad
    else
        focus_app
        "$CONFIG_DIR/size_and_center_float.sh" "$APP_NAME"
    fi
    sketchybar --trigger scratchpad_update
}
main