#!/usr/bin/env bash

ram=(
    icon=
    # icon.color=$ORANGE
    update_freq=5
    script="$PLUGIN_DIR/ram.sh"
    click_script="$SCRIPTS_DIR/run_external_bash.sh '/opt/homebrew/bin/btop && exit'"
)

sketchybar --add item ram right \
    --set ram "${ram[@]}"