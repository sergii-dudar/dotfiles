input=(
    "${right_items_common[@]}"
    icon.padding_right=1
    icon.y_offset=3
    label.color="$INPUT_LABEL_COLOR"
    script="$PLUGIN_DIR/right/input.sh"
)

sketchybar --add item input right \
    --set input "${input[@]}" \
    --add event input_change 'AppleSelectedInputSourcesChangedNotification' \
    --subscribe input input_change