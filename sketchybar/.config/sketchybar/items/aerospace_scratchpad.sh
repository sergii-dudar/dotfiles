#!/usr/bin/env bash

scratchpad=(
    icon=
    label=0
    update_freq=0
    script="$PLUGIN_DIR/aerospace_scratchpad.sh"
)

sketchybar --add event scratchpad_update \
    --add item scratchpad left \
    --set scratchpad "${scratchpad[@]}" \
    --subscribe scratchpad scratchpad_update