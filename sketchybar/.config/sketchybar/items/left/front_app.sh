# chevron=(
#     "${left_items_common[@]}"
#     icon="$FRONT_APP_SEPARATOR"
#     label.drawing=off
#     background.drawing=off
# )

front_app=(
    "${left_items_common[@]}"
    background.drawing=off
    icon.font="$ICON_APP_FONT"
    label.color="$FRONT_APP_LABEL_COLOR"
    icon.color="$FRONT_APP_ICON_COLOR"
    script="$PLUGIN_DIR/left/front_app.sh"
)

sketchybar --add item front_app left \
    --set front_app "${front_app[@]}" \
    --subscribe front_app front_app_switched

# --add item chevron left \
    # --set chevron "${chevron[@]}" \