#!/bin/bash

sketchybar --add item break_button center \
           --set break_button label="☕️" \
                 drawing=off \
                 click_script="NAME=break_button $PLUGIN_DIR/pomodoro.sh"
