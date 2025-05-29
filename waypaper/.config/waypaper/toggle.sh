#!/usr/bin/env bash

wm_name="${1:-}"

if pgrep waypaper; then
    killall waypaper
    exit 0
fi

case "$wm_name" in
    sway)
        waypaper --backend swaybg
        ;;
    hyprland)
        # first_monitor=$(hyprctl monitors | grep "Monitor " | head -n 1 | awk '{ print $2 }')
        waypaper --backend hyprpaper
        ;;
    *)
        ;;
esac