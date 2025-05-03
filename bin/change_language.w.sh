#!/usr/bin/env bash

CURRENT_LAYOUT=$(swaymsg -t get_inputs | jq -r '.[] | select(.type=="keyboard") | .xkb_active_layout_index' | head -n 1)

if [ "$CURRENT_LAYOUT" = 0 ]; then
    swaymsg input type:keyboard xkb_switch_layout 1
    notify-send "Lang: UA ""🇺🇦" -t 700
else
    swaymsg input type:keyboard xkb_switch_layout 0
    notify-send "Lang: US ""🇺🇸" -t 700
fi