CURRENT_MODE="$(aerospace list-modes --current)"
keymode=(
    "${left_items_common[@]}"
    background.padding_right=2
    icon="$KEYBOARD"
    icon.y_offset=2.5
    label="$CURRENT_MODE"
    label.font.style="Bold Italic"
    update_freq=0
    icon.color="$MAGENTA"
    label.color="$BLUE"
    script="$PLUGIN_DIR/right/aerospace_keymode.sh"
    drawing="off"
)

sketchybar --add event keymode_update \
    --add item keymode right \
    --set keymode "${keymode[@]}" \
    --subscribe keymode keymode_update