#!/bin/bash

date=(
    width=0
    y_offset=5
    label.font.size=10
    update_freq=60
    script='sketchybar --set $NAME label="$(date +"%a %d %b")"'
)
time=(
    label.width=60
    y_offset=-5
    label.align=left
    label.font.size=12.5
    update_freq=1
    script='sketchybar --set $NAME label="$(date "+%T")"'
)

sketchybar \
    --add item date right \
    --set date "${date[@]}" \
    \
    --add item time right \
    --set time "${time[@]}"
