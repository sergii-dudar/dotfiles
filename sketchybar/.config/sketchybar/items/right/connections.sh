#!/usr/bin/env bash

wifi=(
    "${right_items_common[@]}"
    background.drawing=off
    icon.padding_left=6
    icon.padding_right=3
    icon.y_offset=2
    label.drawing=off
    icon="󰤭 "
    script="$PLUGIN_DIR/right/wifi.sh"
)
bluetooth=(
    "${right_items_common[@]}"
    background.drawing=off
    icon.padding_left=3
    icon.padding_right=6
    label.drawing=off
    update_freq=10
    script="$PLUGIN_DIR/right/bluetooth.sh"
    click_script="open \"x-apple.systempreferences:com.apple.BluetoothSettings\""
)

# Bluetooth Status
sketchybar --add item group_connection_bluetooth right --set group_connection_bluetooth "${bluetooth[@]}" \
    --add item group_connection_wifi right --set group_connection_wifi "${wifi[@]}" \
    --subscribe group_connection_wifi wifi_change mouse.clicked \
    --add bracket connections_group '/group_connection_.*/'