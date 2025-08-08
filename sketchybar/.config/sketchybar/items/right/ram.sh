#!/usr/bin/env bash

ram=(
    "${right_items_common[@]}"
    icon=" " # 
    # icon.color=$ORANGE
    update_freq=10
    script="$PLUGIN_DIR/right/ram.sh"
    click_script="$SCRIPTS_DIR/run_external_bash.sh '/opt/homebrew/bin/btop && exit'"
)

sketchybar --add item ram right \
    --set ram "${ram[@]}"