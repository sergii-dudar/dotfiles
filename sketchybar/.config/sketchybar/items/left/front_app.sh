#!/usr/bin/env bash

chevron=(
    "${left_items_common[@]}"
    icon=""
    label.drawing=off
    background.drawing=off
)

front_app=(
    "${left_items_common[@]}"
    icon.font="sketchybar-app-font:Regular:16.0" \
        script="$PLUGIN_DIR/left/front_app.sh" \
    )

sketchybar --add item chevron left \
    --set chevron "${chevron[@]}" \
    --add item front_app left \
    --set front_app "${front_app[@]}" \
    --subscribe front_app front_app_switched