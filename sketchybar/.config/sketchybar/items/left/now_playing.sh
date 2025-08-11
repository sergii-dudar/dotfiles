now_playing=(
    "${left_items_common[@]}"
    update_freq=5
    label.font.style="Bold Italic"
    script="$PLUGIN_DIR/left/now_playing.sh"
    background.drawing=on
    label.color="$PLAYER_LABLE_COLOR"
    drawing=off
    display=1
)

# Now Playing
sketchybar --add item now_playing left \
    --set now_playing "${now_playing[@]}" \
    --add event now_playing_update \
    --subscribe now_playing now_playing_update media_change mouse.clicked mouse.scrolled

# `now_playing_update` event triggering in karabiner elements config after changing MPD player controlling

# click_script="mpc toggle ; echo $SENDER > /tmp/logs.txt" \
    #click_script="mpc toggle" \
    # icon.font="SF Pro:Semibold:15.0" \
    # label.font="SF Pro:Medium:12.0" \
    # background.color=$BACKGROUND_1 \