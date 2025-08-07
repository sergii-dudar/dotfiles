#!/usr/bin/env bash

INPUT_NAME=$(SwitchAudioSource -t input -c)
VOLUME=$(osascript -e 'input volume of (get volume settings)')

case $INPUT_NAME in
    'MacBook Pro Microphone')
        INPUT_NAME="Mic"
        ;;
    'USB PnP Audio Device')
        INPUT_NAME="USB"
        ;;
    'HD Pro Webcam C920')
        INPUT_NAME="USB"
        ;;
esac

ICON="􀊰 "
HIGHLIGH=off
if [ "$VOLUME" -eq 0 ]; then
    ICON=􀊲
    HIGHLIGH=on
fi

sketchybar --set "$NAME" \
    label="$VOLUME%" \
    icon="$ICON" \
    icon.highlight=$HIGHLIGH
