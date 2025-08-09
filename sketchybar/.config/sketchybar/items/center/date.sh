sketchybar --add item date center \
    --set date "${center_items_common[@]}" \
    update_freq=60 \
    icon="$CALENDAR" \
    icon.y_offset=2 \
    icon.color="$DATE_ICON_COLOR" \
    label.color="$DATE_LABEL_COLOR" \
    background.corner_radius=3 \
    background.padding_right=5 \
    script="$PLUGIN_DIR/center/date.sh" \
    click_script="open -a Calendar"