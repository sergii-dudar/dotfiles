sketchybar --add item zen left \
    --set media \
    label.max_chars=30 \
    icon.padding_left=0 \
    scroll_texts=on \
    icon=􀑪  \
    background.drawing=off \
    script="$PLUGIN_DIR/media.sh" \
    --subscribe media media_change


# label.color=$ACCENT_COLOR \
    # icon.color=$ACCENT_COLOR   \

    # media=(
#     icon=
#     # icon.color=$SKY
#     script="$PLUGIN_DIR/media.sh"
#     label.max_chars=35
#     scroll_texts=on
#     updates=on
#     drawing=off
# )
#
# sketchybar --add item media left \
    #     --set media "${media[@]}" \
    #     --subscribe media media_change
#