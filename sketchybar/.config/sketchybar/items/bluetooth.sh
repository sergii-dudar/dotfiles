#!/usr/bin/env bash

# Bluetooth Status
sketchybar --add item bluetooth right \
    --set bluetooth icon.font="SF Pro:Semibold:15.0" \
    icon.padding_left=9 \
    icon.padding_right=9 \
    label.drawing=off \
    update_freq=10 \
    script="$PLUGIN_DIR/bluetooth.sh" \
    background.drawing=on \
    padding_left=4 \
    padding_right=4


# background.color=$BACKGROUND_1 \