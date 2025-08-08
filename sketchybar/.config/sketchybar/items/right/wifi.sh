#!/usr/bin/env bash

wifi=(
    "${right_items_common[@]}"
    # padding_right=0
    # padding_left=0
    icon.padding_left=5
    icon.padding_right=6
    label.drawing=off
    icon="󰤭 "
    script="$PLUGIN_DIR/right/wifi.sh"
)

sketchybar --add item wifi right \
    --set wifi "${wifi[@]}" \
    --subscribe wifi wifi_change mouse.clicked

# sketchybar --add item wifi right \
    #     --set wifi \
    #     label.drawing=off \
    #     update_freq=10 \
    #     script="$PLUGIN_DIR/wifi.sh" \
    #     background.drawing=on \
    #     drawing=off \
    #     \
    #     --add item ethernet right \
    #     --set ethernet \
    #     label.drawing=off \
    #     update_freq=10 \
    #     script="$PLUGIN_DIR/ethernet.sh" \
    #     background.drawing=on \
    #     drawing=off




# background.color=$BACKGROUND_1 \

    # wifi=(
#     update_freq=5
#     script="$PLUGIN_DIR/wifi.sh"
# )
# vpn=(
#     update_freq=5
#     script="$PLUGIN_DIR/vpn.sh"
# )
#
# sketchybar --add item wifi right \
    #     --set wifi "${wifi[@]}" \
    #     --subscribe wifi wifi_change \
    #     \
    #     --add item vpn right \
    #     --set vpn "${vpn[@]}"