#!/bin/bash

cpu=(
    icon=
    icon.color=$GREEN
    update_freq=3
    script="$PLUGIN_DIR/cpu.sh"
)

sketchybar --add item cpu left \
    --set cpu "${cpu[@]}"
