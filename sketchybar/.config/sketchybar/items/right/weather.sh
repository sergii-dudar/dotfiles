#!/usr/bin/env bash

weather=(
    "${right_items_common[@]}"
    script="$PLUGIN_DIR/right/weather.sh"
    click_script="open -a Weather"
)

sketchybar --add item weather right \
    --set weather "${weather[@]}" \
    update_freq=300