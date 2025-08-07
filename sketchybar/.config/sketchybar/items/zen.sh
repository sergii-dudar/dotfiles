#!/usr/bin/env bash

sketchybar --add item media left \
    --set media \
    label.max_chars=30 \
    update_freq=0 \
    icon.padding_left=0 \
    scroll_texts=on \
    icon="󰛐 "  \
    background.drawing=off \
    script="$PLUGIN_DIR/zen.sh"
