#!/usr/bin/env bash

COUNT="$(aerospace list-windows --workspace "NSP" 2>/dev/null | wc -l | awk '{print $1}')"

sketchybar --set "$NAME" \
    label="$COUNT"
