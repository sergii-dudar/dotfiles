#!/usr/bin/env bash

. "$HOME"/dotfiles/bin/wmscripts/status-bar/shared-openweather.cached

emoji_icon=$(get_emoji2 "$WEATHER_ICON")

sketchybar -m \
    --set "$NAME" icon="$emoji_icon" \
    --set "$NAME" label="$TEMPERATURE"°C