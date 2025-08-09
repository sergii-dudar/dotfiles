#!/usr/bin/env bash

sketchybar --add item clock center \
    --set clock "${center_items_common[@]}" \
    update_freq=10 \
    icon="$CLOCK" \
    icon.y_offset=1.5 \
    icon.color=0xffbd93f9 \
    label.color=0xff8caaee \
    background.corner_radius=3 \
    background.padding_left=5 \
    script="$PLUGIN_DIR/center/clock.sh" \
    click_script="open -a Clock"