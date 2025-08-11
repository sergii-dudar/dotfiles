battery=(
    "${right_items_common[@]}"
    update_freq=120
    icon.color="$BATTERY_ICON_COLOR"
    icon.y_offset=2
    icon.font.size=19
    script="$PLUGIN_DIR/right/battery.sh"
    click_script="open \"x-apple.systempreferences:com.apple.Battery-Settings.extension\""
    display=1
)

sketchybar --add item battery right \
    --set battery "${battery[@]}" \
    --subscribe battery system_woke power_source_change