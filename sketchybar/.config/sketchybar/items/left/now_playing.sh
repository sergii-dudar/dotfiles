#!/usr/bin/env bash

now_playing=(
    "${left_items_common[@]}"
    update_freq=5
    label.font.style="Bold Italic"
    script="$PLUGIN_DIR/left/now_playing.sh"
    background.drawing=on
    drawing=off
)

# Now Playing
sketchybar --add item now_playing left \
    --set now_playing "${now_playing[@]}" \
    --subscribe now_playing media_change mouse.clicked mouse.scrolled

# click_script="mpc toggle ; echo $SENDER > /tmp/logs.txt" \
    #click_script="mpc toggle" \
    # icon.font="SF Pro:Semibold:15.0" \
    # label.font="SF Pro:Medium:12.0" \
    # background.color=$BACKGROUND_1 \