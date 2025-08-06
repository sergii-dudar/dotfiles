#!/usr/bin/env bash

sketchybar --add item date center \
    --set date \
    update_freq=60 \
    icon=" " \
    icon.color=0xff7c8377 \
    icon.padding_left=10 \
    label.color=0xff6272a4 \
    label.padding_right=10 \
    background.color=0xff2e3440 \
    background.corner_radius=3 \
    background.padding_right=5 \
    script="$PLUGIN_DIR/date.sh"