#!/usr/bin/env bash

# "format": "<span color='#7c8377'> </span><span color='#6272a4'>{:%a, %b %d}</span>",
sketchybar --set "$NAME" label="$(date '+%a, %b %d')"