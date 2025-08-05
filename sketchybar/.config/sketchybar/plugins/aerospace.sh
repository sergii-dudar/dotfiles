#!/usr/bin/env bash

# make sure it's executable with:
# chmod +x ~/.config/sketchybar/plugins/aerospace.sh

if [ "$1" = "$FOCUSED_WORKSPACE" ]; then
    sketchybar --set $NAME background.drawing=on background.color=0x44ffffff
elif [ $(aerospace list-windows --workspace "$1" | wc -l | awk '{print $1}') -gt 0 ] && [ "$1" != "$FOCUSED_WORKSPACE" ]; then
    sketchybar --set $NAME background.drawing=on background.color=0xffa6da95
else
    sketchybar --set $NAME background.drawing=off
fi