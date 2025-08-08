#!/usr/bin/env bash

input=(
    "${right_items_common[@]}"
    script="$PLUGIN_DIR/right/input.sh"
)

sketchybar --add item input right \
    --set input "${input[@]}" \
    --add event input_change 'AppleSelectedInputSourcesChangedNotification' \
    --subscribe input input_change