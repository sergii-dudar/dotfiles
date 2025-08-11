disk=(
    "${right_items_common[@]}"
    icon="$SYSTEM_DISK"
    icon.font.size=20
    icon.color="$DISK_ICON_COLOR"
    update_freq=300
    script="$PLUGIN_DIR/right/disk.sh"
    display=1
)

sketchybar --add item disk right \
    --set disk "${disk[@]}" \
    --subscribe disk mouse.clicked