#!/usr/bin/env bash




wifi=(
    padding_right=6
    padding_left=1
    label.width=0
    icon="󰤭 "
    script="$PLUGIN_DIR/wifi.sh"
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