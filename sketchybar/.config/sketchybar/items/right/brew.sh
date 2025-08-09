#!/usr/bin/env bash

# Trigger the brew_udpate event when brew update or upgrade is run from cmdline
# e.g. via function in .zshrc

brew=(
    "${right_items_common[@]}"
    icon.y_offset=2
    icon="$PACKAGES_SYNC"
    label="$PACKAGES_SYNC_OK"
    # Set update frequency to 30 min (30*60=1800)
    update_freq=1800
    script="$PLUGIN_DIR/right/brew.sh"
)

sketchybar --add event brew_update \
    --add item brew right \
    --set brew "${brew[@]}" \
    --subscribe brew brew_update mouse.clicked
