#!/usr/bin/env bash

# WiFi Status
sketchybar --add item wifi right \
    --set wifi \
    icon.padding_left=8 \
    icon.padding_right=8 \
    label.drawing=off \
    update_freq=10 \
    script="$PLUGIN_DIR/wifi.sh" \
    background.drawing=on \
    padding_left=4 \
    padding_right=4

# background.color=$BACKGROUND_1 \