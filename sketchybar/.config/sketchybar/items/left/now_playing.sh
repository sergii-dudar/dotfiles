now_playing=(
    "${left_items_common[@]}"
    update_freq=5
    label.font.style="Bold Italic"
    script="$PLUGIN_DIR/left/now_playing.sh"
    background.drawing=on
    label.color="$PLAYER_LABLE_COLOR"
    drawing=off
)

# Now Playing
sketchybar --add item now_playing left \
    --set now_playing "${now_playing[@]}" \
    --add event now_playing_update \
    --subscribe now_playing now_playing_update media_change mouse.clicked mouse.scrolled

# click_script="mpc toggle ; echo $SENDER > /tmp/logs.txt" \
    #click_script="mpc toggle" \
    # icon.font="SF Pro:Semibold:15.0" \
    # label.font="SF Pro:Medium:12.0" \
    # background.color=$BACKGROUND_1 \