weather=(
    "${right_items_common[@]}"
    icon.y_offset=2
    icon.padding_right=2
    script="$PLUGIN_DIR/right/weather.sh"
    click_script="open -a Weather"
)

sketchybar --add item weather right \
    --set weather "${weather[@]}" \
    update_freq=300