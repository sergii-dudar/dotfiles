#!/usr/bin/env bash

# Now Playing
sketchybar --add item now_playing left \
    --set now_playing \
    icon.padding_left=8 \
    icon.padding_right=6 \
    label.padding_left=4 \
    label.padding_right=8 \
    update_freq=2 \
    script="$PLUGIN_DIR/now_playing.sh" \
    background.drawing=on \
    drawing=off \
    padding_left=4 \
    padding_right=4 \
    --subscribe now_playing media_change mouse.clicked mouse.scrolled

# click_script="mpc toggle ; echo $SENDER > /tmp/logs.txt" \
    #click_script="mpc toggle" \
    # icon.font="SF Pro:Semibold:15.0" \
    # label.font="SF Pro:Medium:12.0" \
    # background.color=$BACKGROUND_1 \