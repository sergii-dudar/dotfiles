brew=(
    "${right_items_common[@]}"
    icon.y_offset=2
    icon="$PACKAGES_SYNC"
    icon.color="$BREW_ICON_COLOR"
    label="$PACKAGES_SYNC_OK"
    # Set update frequency to 30 min (30*60=1800)
    update_freq=1800
    script="$PLUGIN_DIR/right/brew.sh"
    display=1
)

sketchybar --add event brew_update \
    --add item brew right \
    --set brew "${brew[@]}" \
    --subscribe brew brew_update mouse.clicked