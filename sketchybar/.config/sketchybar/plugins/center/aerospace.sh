#!/usr/bin/env bash

# echo "$1, $FOCUSED_WORKSPACE" > /tmp/logs.txt

source "$CONFIG_DIR/colors.sh"

ws_clients_number="$(aerospace list-windows --workspace "$1" 2>/dev/null | wc -l | awk '{print $1}')"

# echo "ws: $1" > /tmp/logs.txt

if [ "$1" = "$FOCUSED_WORKSPACE" ] || [ "$1" = "$(aerospace list-workspaces --focused)" ]; then
    # Active ws
    sketchybar --set "$NAME" \
        icon.color="$WS_ACTIVE_ICON_COLOR" \
        label.color="$WS_ACTIVE_LABEL_COLOR" \
        background.drawing=on \
        background.color="$WS_ACTIVE_BG_COLOR" \
        background.height=25 \
        background.y_offset=0 \
        drawing=on
elif [ "$ws_clients_number" -gt 0 ] && [ "$(aerospace list-workspaces --monitor 2)" = "$1" ] && [ "$1" != "$FOCUSED_WORKSPACE" ]; then
    # Not active second monitor ws with clients
    sketchybar --set "$NAME" \
        icon.color="$WS_ACTIVE_ANOTHER_DISPLAY_ICON_COLOR" \
        label.color="$WS_ACTIVE_ANOTHER_DISPLAY_LABEL_COLOR" \
        background.drawing=on \
        background.color="$WS_ACTIVE_ANOTHER_DISPLAY_BG_COLOR" \
        background.height=3 \
        background.y_offset=-10 \
        drawing=on
elif [ "$ws_clients_number" -gt 0 ] && [ "$1" != "$FOCUSED_WORKSPACE" ]; then
    # Not active ws with clients
    sketchybar --set "$NAME" \
        icon.color="$WS_WIHT_CLIENTS_LABEL_COLOR" \
        label.color="$WS_WIHT_CLIENTS_ICON_COLOR" \
        background.drawing=on \
        background.color="$WS_WIHT_CLIENTS_BG_COLOR" \
        background.height=3 \
        background.y_offset=-10 \
        drawing=on
else
    # Default
    sketchybar --set "$NAME" \
        label.color="$WS_DEFAULT_COLOR" \
        icon.color="$WS_DEFAULT_COLOR" \
        background.drawing=off \
        drawing=off
    # remove `drawing=off` in case you want to render empty workspaces also.
fi