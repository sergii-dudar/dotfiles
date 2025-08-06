#!/usr/bin/env bash

STATE="$(echo "$INFO" | jq -r '.state')"
if [ "$STATE" = "playing" ]; then
    MEDIA="$(echo "$INFO" | jq -r '.title + " - " + .artist')"
    sketchybar --set $NAME label="$MEDIA" drawing=on
    echo "$INFO, on" > /tmp/logs.txt
else
    sketchybar --set $NAME drawing=off
    echo "$INFO, off" > /tmp/logs.txt
fi

# update_media() {
#     STATE="$(echo "$INFO" | jq -r '.state')"
#
#     if [ "$STATE" = "playing" ]; then
#         MEDIA="$(echo "$INFO" | jq -r '.artist + " - " + .title')"
#         sketchybar --set $NAME label="$MEDIA" drawing=on
#     else
#         sketchybar --set $NAME drawing=off
#     fi
# }
#
# case "$SENDER" in
#     "media_change")
#         update_media
#         ;;
# esac


