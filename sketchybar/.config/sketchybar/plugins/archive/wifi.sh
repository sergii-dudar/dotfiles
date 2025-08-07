#!/usr/bin/env bash

IP="$(ipconfig getsummary en0 | grep -o "yiaddr = .*" | sed 's/^yiaddr = //')"

ICON=􀙈
HIGHLIGHT=on
if [ -n "$IP" ]; then
    ICON=􀙇
    HIGHLIGHT=off
fi

sketchybar --set $NAME icon=$ICON icon.highlight=$HIGHLIGHT

# # Set icon and color based on connection status
# if [[ "$WIFI_CONNECTED" == true ]]; then
#     # WiFi is connected - green icon
#     ICON="󰤨"
#     COLOR=$ACCENT_SECONDARY  # Green
# else
#     # WiFi is disconnected - gray icon
#     ICON="󰤭"
#     COLOR=$GREY
# fi
