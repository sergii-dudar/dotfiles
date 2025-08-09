#!/usr/bin/env bash

LABEL=$(df -H | grep -E '^(/dev/disk3s5).' | awk '{ printf ("%s\n", $5) }')
sketchybar --set "$NAME" label="$LABEL"