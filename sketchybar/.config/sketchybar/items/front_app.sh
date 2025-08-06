#!/usr/bin/env bash

sketchybar --add item chevron left \
    --set chevron icon= label.drawing=off padding_left=15 \
    --add item front_app left \
    --set front_app icon.drawing=off script="$PLUGIN_DIR/front_app.sh" \
    --subscribe front_app front_app_switched
