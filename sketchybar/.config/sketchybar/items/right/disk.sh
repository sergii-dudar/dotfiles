#!/usr/bin/env bash

disk=(
    "${right_items_common[@]}"
    icon="$SYSTEM_DISK"
    icon.font.size=20
    # icon.color=$BLUE
    update_freq=300
    script="$PLUGIN_DIR/right/disk.sh"
    click_script="$SCRIPTS_DIR/run_external_bash.sh '/opt/homebrew/bin/gdu-go ~ && exit'"
)

sketchybar --add item disk right \
    --set disk "${disk[@]}"