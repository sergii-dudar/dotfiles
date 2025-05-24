#!/usr/bin/env bash

wm_name="${1:-}"

case "$wm_name" in
    sway)
        CURRENT_LAYOUT=$(swaymsg -t get_inputs | jq -r '.[] | select(.type=="keyboard") | .xkb_active_layout_index' | head -n 1)
        if [ "$CURRENT_LAYOUT" = 0 ]; then
            swaymsg input type:keyboard xkb_switch_layout 1
            notify-send "Lang: UA ""🇺🇦" -t 700
        else
            swaymsg input type:keyboard xkb_switch_layout 0
            notify-send "Lang: US ""🇺🇸" -t 700
        fi
        ;;
    hyprland)
        # hyprctl switchxkblayout all next
        # hyprctl switchxkblayout current next
        CURRENT_LAYOUT=$(hyprctl devices -j | jq -r '.keyboards[] | .active_keymap' | tail -n1 | cut -c1-2 | tr '[:lower:]' '[:upper:]')
        # echo "layout $CURRENT_LAYOUT"
        if [ "$CURRENT_LAYOUT" = "EN" ]; then
            hyprctl switchxkblayout current 1
            notify-send "Lang: UA ""🇺🇦" -t 700
        else
            hyprctl switchxkblayout current 0
            notify-send "Lang: US ""🇺🇸" -t 700
        fi
        ;;
    *)
        CURRENT_LAYOUT=$(setxkbmap -query | awk -F : 'NR==3{print $2}' | sed 's/ //g')
        if [ "$CURRENT_LAYOUT" = "us" ]; then
            setxkbmap "ua"
            notify-send "Lang: UA ""🇺🇦" -t 700
        else
            setxkbmap "us"
            notify-send "Lang: US ""🇺🇸" -t 700
        fi
        ;;
esac