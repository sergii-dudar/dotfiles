#!/usr/bin/env bash

sketchybar --add item weather right \
    --set weather  \
    script="$PLUGIN_DIR/weather.sh" \
    click_script="open -a Weather" \
    update_freq=300