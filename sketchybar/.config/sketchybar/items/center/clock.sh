#!/usr/bin/env bash

sketchybar --add item clock center \
    --set clock \
    update_freq=10 \
    icon=" " \
    icon.color=0xffbd93f9 \
    icon.padding_left=10 \
    label.color=0xff8caaee \
    label.padding_right=10 \
    background.corner_radius=3 \
    background.padding_left=5 \
    script="$PLUGIN_DIR/center/clock.sh" \
    click_script="open -a Clock"