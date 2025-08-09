POPUP_OFF="sketchybar --set apple.logo popup.drawing=off"
POPUP_CLICK_SCRIPT="sketchybar --set \$NAME popup.drawing=toggle"

popup_props=(
    popup.height=30
)
apple_logo=(
    "${left_items_common[@]}"
    "${popup_props[@]}"
    icon="$APPLE"
    icon.color="$RED"
    icon.font.size=18
    label.drawing=off
    icon.y_offset=2
    click_script="$POPUP_CLICK_SCRIPT"
)
pupup_item=(
    # icon.padding_left=6
    icon.padding_right=6
    # label.padding_left=0
    # label.padding_right=6
    background.padding_right=10
    background.padding_left=10
    background.drawing=off
)

sketchybar --add item apple.logo left \
    --set apple.logo "${apple_logo[@]}" \
    \
    --add item apple.prefs popup.apple.logo \
    --set apple.prefs "${pupup_item[@]}" \
    icon="$PREFERENCES" \
    label="Preferences" \
    icon.color="$WHITE" \
    label.color="$WHITE" \
    click_script="open -a 'System Preferences' ; $POPUP_OFF" \
    \
    --add item apple.activity popup.apple.logo \
    --set apple.activity "${pupup_item[@]}" \
    icon="$ACTIVITY" \
    label="Activity" \
    icon.color="$WHITE" \
    label.color="$WHITE" \
    click_script="open -a 'Activity Monitor' ; $POPUP_OFF" \
    \
    --add item apple.lock popup.apple.logo \
    --set apple.lock "${pupup_item[@]}" \
    icon="$LOCK" \
    label="Lock Screen" \
    icon.color="$WHITE" \
    label.color="$WHITE" \
    click_script="pmset displaysleepnow ; $POPUP_OFF"