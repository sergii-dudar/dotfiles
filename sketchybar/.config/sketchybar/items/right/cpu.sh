#!/usr/bin/env bash

# sketchybar --add item cpu.percent right \
    #     --set cpu.percent \
    #     icon="$SYSTEM_CPU" \
    #     label="8%" \
    #     icon.font="CaskaydiaCove Nerd Font:Bold:17.0" \
    #     label.font="CaskaydiaCove Nerd Font:Bold:16.0" \
    #     update_freq=5 \
    #     mach_helper="$HELPER"


cpu=(
    "${right_items_common[@]}"
    icon="$SYSTEM_CPU"
    icon.color="0xff8caaee"
    update_freq=10
    script="$PLUGIN_DIR/right/cpu.sh"
    click_script="open -a Activity\ Monitor"
)

sketchybar --add item cpu right \
    --set cpu "${cpu[@]}"