#!/bin/sh

sketchybar -m --add item mpd center \
              --set mpd update_freq=2 \
              --set mpd script="$PLUGIN_DIR/mpd.sh" \
              --set mpd click_script="mpc toggle"