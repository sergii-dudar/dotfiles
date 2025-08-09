#!/usr/bin/env bash

scratchpad=(
    "${left_items_common[@]}"
    background.padding_right=2
    icon="$WINDOW_FLOAT"
    icon.y_offset=2.5
    icon.color="0xff6272a4"
    label=0
    label.color="0xffbd93f9"
    update_freq=0
    script="$PLUGIN_DIR/left/aerospace_scratchpad.sh"
    click_script="aerospace workspace NSP"
)

sketchybar --add event scratchpad_update \
    --add item scratchpad left \
    --set scratchpad "${scratchpad[@]}" \
    --subscribe scratcsketchybarhpad scratchpad_update