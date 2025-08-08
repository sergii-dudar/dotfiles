#!/usr/bin/env bash

scratchpad=(
    "${left_items_common[@]}"
    icon=" "
    label=0
    update_freq=0
    script="$PLUGIN_DIR/left/aerospace_scratchpad.sh"
)

sketchybar --add event scratchpad_update \
    --add item scratchpad left \
    --set scratchpad "${scratchpad[@]}" \
    --subscribe scratchpad scratchpad_update