#!/bin/bash

disk=(
    icon=󱛟
    # icon.color=$BLUE
    update_freq=300
    script="$PLUGIN_DIR/disk.sh"
)

sketchybar --add item disk right \
    --set disk "${disk[@]}"