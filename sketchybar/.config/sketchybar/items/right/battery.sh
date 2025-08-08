#!/usr/bin/env bash

battery=(
    "${right_items_common[@]}"
    update_freq=120
    script="$PLUGIN_DIR/right/battery.sh"
    click_script="open \"x-apple.systempreferences:com.apple.Battery-Settings.extension\""
)

sketchybar --add item battery right \
    --set battery "${battery[@]}" \
    --subscribe battery system_woke power_source_change