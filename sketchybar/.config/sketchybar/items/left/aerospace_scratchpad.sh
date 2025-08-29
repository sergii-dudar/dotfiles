scratchpad=(
    "${left_items_common[@]}"
    background.padding_right=2
    icon="$WINDOW_FLOAT"
    icon.y_offset=2.5
    label=0
    update_freq=0
    icon.color="$SCRATCHPAD_ICON_COLOR"
    label.color="$SCRATCHPAD_LABEL_COLOR"
    script="$PLUGIN_DIR/left/aerospace_scratchpad.sh"
    click_script="aerospace workspace NSP"
)

sketchybar --add event scratchpad_update \
    --add item scratchpad left \
    --set scratchpad "${scratchpad[@]}" \
    --subscribe scratchpad "${popup_events[@]}" scratchpad_update mouse.clicked