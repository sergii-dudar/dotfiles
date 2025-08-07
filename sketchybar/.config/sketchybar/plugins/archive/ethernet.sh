#!/usr/bin/env bash

if networksetup -getinfo "USB 10/100/1000 LAN" | rg -q -i "IP address: [\d\.]+"; then
    # Ethernet wired is connected - green icon
    ICON="󰈀 󰈁"
    COLOR=$ACCENT_SECONDARY  # Green
    DRAWING=on
else
    # Ethernet wired disconnected - gray icon
    ICON="󰈂 "
    COLOR=$GREY
    DRAWING=off
fi

sketchybar --set "$NAME" icon="$ICON" \
    label.drawing=off \
    drawing="$DRAWING"


# icon.color="$COLOR" \