#!/usr/bin/env bash

LAYOUT_FILE="/tmp/sway-layout-toggle"
CURRENT_LAYOUT=$(swaymsg -t get_tree | jq -r '.. | objects | select(.focused==true and .type=="con") | .layout // empty')

if [[ "$CURRENT_LAYOUT" == "tabbed" ]]; then
    swaymsg layout splith
    echo "splith" > "$LAYOUT_FILE"
elif [[ "$CURRENT_LAYOUT" == "splith" ]]; then
    swaymsg layout tabbed
    echo "tabbed" > "$LAYOUT_FILE"
else
    # Fallback using the last known layout
    if [[ -f "$LAYOUT_FILE" ]]; then
        LAST_LAYOUT=$(<"$LAYOUT_FILE")
        if [[ "$LAST_LAYOUT" == "tabbed" ]]; then
            swaymsg layout splith
            echo "splith" > "$LAYOUT_FILE"
        else
            swaymsg layout tabbed
            echo "tabbed" > "$LAYOUT_FILE"
        fi
    else
        # Default to tabbed first time
        swaymsg layout tabbed
        echo "tabbed" > "$LAYOUT_FILE"
    fi
fi