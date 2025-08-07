#!/usr/bin/env bash

# Bluetooth Status
sketchybar --add item bluetooth right \
    --set bluetooth \
    label.drawing=off \
    update_freq=10 \
    script="$PLUGIN_DIR/bluetooth.sh" \
    background.drawing=on \
    drawing=off



# background.color=$BACKGROUND_1 \