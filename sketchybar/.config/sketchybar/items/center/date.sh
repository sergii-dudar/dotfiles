#!/usr/bin/env bash

sketchybar --add item date center \
    --set date "${center_items_common[@]}" \
    update_freq=60 \
    icon="$CALENDAR" \
    icon.color=0xff7c8377 \
    icon.y_offset=2 \
    label.color=0xff6272a4 \
    background.corner_radius=3 \
    background.padding_right=5 \
    script="$PLUGIN_DIR/center/date.sh" \
    click_script="open -a Calendar"