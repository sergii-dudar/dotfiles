#!/usr/bin/env bash

if [ "$SENDER" = "front_app_switched" ]; then
    sketchybar --set "$NAME" \
        label="$INFO" \
        icon="$("$CONFIG_DIR"/scripts/icon_map_fn.sh "$INFO")"
fi