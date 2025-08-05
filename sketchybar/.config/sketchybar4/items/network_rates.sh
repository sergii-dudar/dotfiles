#!/bin/bash

SIZE=11
WIDTH=70

up=(
    width=0
    label.width=$WIDTH
    label.align=right
    update_freq=3
    y_offset=5
    icon=⇡
    label.font.size=$SIZE
    icon.font.size=$SIZE
    icon.color=$GREEN
    script="$PLUGIN_DIR/network_rates.sh"
)
down=(
    label.width=$WIDTH
    label.align=right
    y_offset=-5
    icon=⇣
    label.font.size=$SIZE
    icon.font.size=$SIZE
    icon.color=$RED
)

sketchybar --add item net.up left \
    --set net.up "${up[@]}" \
    \
    --add item net.down left \
    --set net.down "${down[@]}"
