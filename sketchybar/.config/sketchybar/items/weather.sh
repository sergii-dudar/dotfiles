#!/usr/bin/env bash

sketchybar --add item weather right \
    --set weather  \
    script="$PLUGIN_DIR/weather.sh" \
    update_freq=300