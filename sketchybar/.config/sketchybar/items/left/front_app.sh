#!/usr/bin/env bash

chevron=(
    "${left_items_common[@]}"
    icon=""
    label.drawing=off
    background.drawing=off
)

front_app=(
    "${left_items_common[@]}"
    background.drawing=off
    icon.font="sketchybar-app-font:Regular:18.0"
    script="$PLUGIN_DIR/left/front_app.sh"
)

sketchybar --add item front_app left \
    --set front_app "${front_app[@]}" \
    --subscribe front_app front_app_switched

# --add item chevron left \
    # --set chevron "${chevron[@]}" \
