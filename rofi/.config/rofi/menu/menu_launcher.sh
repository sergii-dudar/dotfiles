#!/usr/bin/env bash

dir="$HOME/.config/rofi/menu"
theme='style-menu'

## Run
if [ ! -t 0 ]; then
    # stdin is piped — use dmenu mode
    rofi -dmenu -theme ${dir}/${theme}.rasi "$@"
else
    rofi -show drun -theme ${dir}/${theme}.rasi "$@"
fi