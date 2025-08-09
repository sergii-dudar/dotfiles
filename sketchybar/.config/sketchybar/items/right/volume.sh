#!/usr/bin/env bash

mic=(
    "${right_items_common[@]}"
    update_freq=10
    icon.y_offset=2
    script="$PLUGIN_DIR/right/mic.sh"
)

volume=(
    "${right_items_common[@]}"
    script="$PLUGIN_DIR/right/volume.sh"
    icon.y_offset=2
)

sketchybar --add item volume right \
    --set volume "${volume[@]}" \
    --subscribe volume volume_change mouse.clicked mouse.scrolled \
    \
    --add item mic right \
    --set mic "${mic[@]}" \
    --subscribe mic mouse.clicked mouse.scrolled