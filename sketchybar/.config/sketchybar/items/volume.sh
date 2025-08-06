#!/usr/bin/env sh

sketchybar --add item volume right \
    --set volume script="$PLUGIN_DIR/volume.sh" \
    --subscribe volume volume_change

# volume=(
#     script="$PLUGIN_DIR/volume.sh"
# )
# mic=(
#     update_freq=1
#     script="$PLUGIN_DIR/mic.sh"
#     click_script="SwitchAudioSource -t input -s \"USB PnP Audio Device\" > /dev/null && osascript -e 'set volume input volume 75'"
# )
#
# sketchybar --add item volume right \
    #     --set volume "${volume[@]}" \
    #     --subscribe volume volume_change \
    #     \
    #     --add item mic right \
    #     --set mic "${mic[@]}"
