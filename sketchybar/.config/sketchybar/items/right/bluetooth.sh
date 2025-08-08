#!/usr/bin/env bash

bluetooth=(
    "${right_items_common[@]}"
    label.drawing=off
    update_freq=10
    script="$PLUGIN_DIR/right/bluetooth.sh"
    click_script="open \"x-apple.systempreferences:com.apple.BluetoothSettings\""
)

# Bluetooth Status
sketchybar --add item bluetooth right \
    --set bluetooth "${bluetooth[@]}" \
    drawing=off



# background.color=$BACKGROUND_1 \