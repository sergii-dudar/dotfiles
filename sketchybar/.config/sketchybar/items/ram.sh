#!/usr/bin/env sh

ram=(
    icon=
    # icon.color=$ORANGE
    update_freq=5
    script="$PLUGIN_DIR/ram.sh"
)

sketchybar --add item ram right \
    --set ram "${ram[@]}"