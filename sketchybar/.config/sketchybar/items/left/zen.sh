#!/usr/bin/env bash

zen=(
    "${left_items_common[@]}"
    label.max_chars=30
    update_freq=0
    icon.padding_left=0
    scroll_texts=on
    icon="$EYE_ON"
    background.drawing=off
)

sketchybar --add item zen left \
    --set zen "${zen[@]}" \
    script="$PLUGIN_DIR/left/zen.sh"