#!/bin/bash

media=(
    icon=
    icon.color=$SKY
    script="$PLUGIN_DIR/media.sh"
    label.max_chars=35
    scroll_texts=on
    updates=on
    drawing=off
)

sketchybar --add item media left \
    --set media "${media[@]}" \
    --subscribe media media_change
