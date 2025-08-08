#!/usr/bin/env bash

# Trigger the brew_udpate event when brew update or upgrade is run from cmdline
# e.g. via function in .zshrc

brew=(
    icon=􀐛
    label=􀆅
    # Set update frequency to 30 min (30*60=1800)
    update_freq=1800
    script="$PLUGIN_DIR/brew.sh"
    click_script="$SCRIPTS_DIR/run_external_bash.sh '/opt/homebrew/bin/brew update && /opt/homebrew/bin/brew upgrade && exit'"
)

sketchybar --add event brew_update \
    --add item brew right \
    --set brew "${brew[@]}" \
    --subscribe brew brew_update