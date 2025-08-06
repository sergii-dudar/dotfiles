#!/bin/bash

VPN_STATUS=$(scutil --nwi | grep -E '^   utun[0-9]')

ICON=􀲊
HIGHLIGHT=on
if [ -n "$VPN_STATUS" ]; then
    ICON=􀙨
    HIGHLIGHT=off
fi

sketchybar --set $NAME icon=$ICON icon.highlight=$HIGHLIGHT
