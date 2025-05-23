#!/usr/bin/env bash

wm_name="${1:-}"
~/dotfiles/bin/change_language.sh "$wm_name"
pkill -RTMIN+2 waybar