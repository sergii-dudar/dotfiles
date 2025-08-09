#!/usr/bin/env bash

# chevron=(
#     "${left_items_common[@]}"
#     icon="$FRONT_APP_SEPARATOR"
#     label.drawing=off
#     background.drawing=off
# )

front_app=(
    "${left_items_common[@]}"
    background.drawing=off
    icon.font="sketchybar-app-font:Regular:18.0"
    label.color="0xff928374"
    icon.color="0xff81A1C1" # "0xff5699af"
    script="$PLUGIN_DIR/left/front_app.sh"
)

sketchybar --add item front_app left \
    --set front_app "${front_app[@]}" \
    --subscribe front_app front_app_switched

# --add item chevron left \
    # --set chevron "${chevron[@]}" \