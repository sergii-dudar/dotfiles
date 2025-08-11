clock=(
    "${center_items_common[@]}"
    update_freq=10
    icon="$CLOCK"
    icon.y_offset=1.5
    icon.color="$CLOCK_ICON_COLOR"
    label.color="$CLOCK_LABEL_COLOR"
    background.corner_radius=3
    background.padding_left=5
    script="$PLUGIN_DIR/center/clock.sh"
    click_script="open -a Clock"
    display=1
)

sketchybar --add item clock center \
    --set clock "${clock[@]}"