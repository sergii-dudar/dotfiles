weather=(
    "${right_items_common[@]}"
    icon.y_offset=2
    icon.padding_right=2
    script="$PLUGIN_DIR/left/weather.sh"
    click_script="open -a Weather"
    display=1
)

sketchybar --add item weather left \
    --set weather "${weather[@]}" \
    update_freq=300