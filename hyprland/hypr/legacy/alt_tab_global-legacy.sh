#!/usr/bin/env bash

active_window=$(hyprctl activewindow -j | jq -r '.address')

# Sort windows by focusHistoryID (most recent first)
# windows=($(hyprctl clients -j | jq -r 'sort_by(.focusHistoryID) | .[].address'))
mapfile -t windows < <(
    hyprctl clients -j |
    jq -r 'sort_by(.focusHistoryID) | .[].address'
)

# Find and focus the first one that is NOT the current window
for addr in "${windows[@]}"; do
    if [[ "$addr" != "$active_window" ]]; then
        hyprctl dispatch focuswindow address:"$addr"
        exit 0
    fi
done