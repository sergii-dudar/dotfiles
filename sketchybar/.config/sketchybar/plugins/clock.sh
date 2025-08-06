#!/usr/bin/env bash

# The $NAME variable is passed from sketchybar and holds the name of
# the item invoking this script:
# https://felixkratz.github.io/SketchyBar/config/events#events-and-scripting

#"format": "<span color='#bd93f9'> </span> <span color='#8caaee'>{:%I:%M %p}</span>",
sketchybar --set "$NAME" label="$(date '+%I:%M %p')"