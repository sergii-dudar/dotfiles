#!/usr/bin/env bash

wifi=(
    "${right_items_common[@]}"
    icon.padding_left=5
    icon.padding_right=6
    icon.y_offset=2
    label.drawing=off
    icon="󰤭 "
    script="$PLUGIN_DIR/right/wifi.sh"
)

sketchybar --add item wifi right \
    --set wifi "${wifi[@]}" \
    --subscribe wifi wifi_change mouse.clicked
