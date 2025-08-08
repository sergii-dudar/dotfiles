#!/usr/bin/env bash

# sketchybar --add item volume right \
    #     --set volume script="$PLUGIN_DIR/volume.sh" \
    #     --subscribe volume volume_change

mic=(
    update_freq=1
    script="$PLUGIN_DIR/mic.sh"
    #click_script="SwitchAudioSource -t input -s \"HD Pro Webcam C920\" > /dev/null && osascript -e 'set volume input volume 75'"
)

sketchybar --add item volume right \
    --set volume script="$PLUGIN_DIR/volume.sh" \
    --subscribe volume volume_change mouse.clicked mouse.scrolled \
    \
    --add item mic right \
    --set mic "${mic[@]}" \
    --subscribe mic mouse.clicked mouse.scrolled