#!/usr/bin/env bash

# echo "$1, $FOCUSED_WORKSPACE" > /tmp/logs.txt

ws_clients_number="$(aerospace list-windows --workspace "$1" | wc -l | awk '{print $1}')"

if [ "$1" = "$FOCUSED_WORKSPACE" ] || [ "$1" = "$(aerospace list-workspaces --focused)" ]; then
    # Active ws
    sketchybar --set "$NAME" \
        icon.color=0xffa6d189 \
        label.color=0xffa6d189 \
        background.drawing=on \
        background.color=0xff44475a \
        background.height=25 \
        background.y_offset=0
elif [ "$ws_clients_number" -gt 0 ] && [ "$(aerospace list-workspaces --monitor 2)" = "$1" ] && [ "$1" != "$FOCUSED_WORKSPACE" ]; then
    # Not active second monitor ws with clients
    sketchybar --set "$NAME" \
        icon.color=0xffbd93f9 \
        label.color=0xffbd93f9 \
        background.drawing=on \
        background.color=0xffb8bb26 \
        background.height=3 \
        background.y_offset=-10
elif [ "$ws_clients_number" -gt 0 ] && [ "$1" != "$FOCUSED_WORKSPACE" ]; then
    # Not active ws with clients
    sketchybar --set "$NAME" \
        icon.color=0xff83a598 \
        label.color=0xff83a598 \
        background.drawing=on \
        background.color=0xff8caaee \
        background.height=3 \
        background.y_offset=-10
else
    # Default
    sketchybar --set "$NAME" \
        label.color=0xff51576d \
        icon.color=0xff51576d \
        background.color=0x00000000
fi