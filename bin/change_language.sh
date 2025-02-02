#!/usr/bin/env bash

CURRENT_LAYOUT=$(setxkbmap -query | awk -F : 'NR==3{print $2}' | sed 's/ //g')

if [ "$CURRENT_LAYOUT" = "us" ]; then
    setxkbmap "ua"
    notify-send "Lang: UA ""🇺🇦" -t 700
else
    setxkbmap "us"
    notify-send "Lang: US ""🇺🇸" -t 700
fi