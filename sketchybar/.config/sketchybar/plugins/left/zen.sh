#!/usr/bin/env bash

zen_on() {
    sketchybar --set apple.logo drawing=on \
        --set cpu drawing=on \
        --set calendar icon.drawing=on \
        --set battery drawing=on \
        --set mic drawing=on \
        --set front_app drawing=on label.drawing=off \
        --set brew drawing=off \
        --set volume drawing=off \
        --set wifi drawing=off \
        --set zen icon="$EYE_OFF"
}

zen_off() {
    sketchybar --set apple.logo drawing=on \
        --set cpu drawing=on \
        --set calendar icon.drawing=on \
        --set battery drawing=on \
        --set mic drawing=on \
        --set front_app drawing=on label.drawing=on \
        --set brew drawing=on \
        --set volume drawing=on \
        --set wifi drawing=on \
        --set zen icon="$EYE_ON"
}

if [ "$1" = "on" ]; then
    zen_on
elif [ "$1" = "off" ]; then
    zen_off
else
    # check for something that is off
    if [ "$(sketchybar --query wifi | jq -r ".geometry.drawing")" = "on" ]; then
        zen_on
    else
        zen_off
    fi
fi