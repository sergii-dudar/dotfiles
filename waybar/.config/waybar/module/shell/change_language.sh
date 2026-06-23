#!/usr/bin/env bash

wm_name="${1:-}"
~/dotfiles/scripts/change_language.sh "$wm_name"
pkill -RTMIN+2 waybar
