#!/usr/bin/env bash

disk=(
    icon=󱛟
    # icon.color=$BLUE
    update_freq=300
    script="$PLUGIN_DIR/disk.sh"
    click_script="$SCRIPTS_DIR/run_external_bash.sh '/opt/homebrew/bin/gdu-go ~ && exit'"
)

sketchybar --add item disk right \
    --set disk "${disk[@]}"