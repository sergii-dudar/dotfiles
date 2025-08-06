#!/usr/bin/env bash

input=(
    script="$PLUGIN_DIR/input.sh"
)

sketchybar --add item input right \
    --set input "${input[@]}" \
    --add event input_change 'AppleSelectedInputSourcesChangedNotification' \
    --subscribe input input_change
