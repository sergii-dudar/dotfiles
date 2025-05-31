#!/usr/bin/env bash

layout_info=$(hyprctl devices -j | jq -r '.keyboards[] | .active_keymap' | tail -n1 | cut -c1-2 | tr '[:lower:]' '[:upper:]')
echo "$layout_info"