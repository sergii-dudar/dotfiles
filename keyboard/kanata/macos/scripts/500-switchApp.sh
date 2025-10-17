#!/usr/bin/env bash

APP="$1"

# Check if there is already a window for the app
ID=$(aerospace list-windows --all --json | jq -r --arg app "$APP" '[.[] | select(."app-name"==$app)][0]."window-id"')

if [[ -n "$ID" && "$ID" != null ]]; then
    # App window exists → focus it
    aerospace focus --window-id "$ID"
else
    # No window → launch and wait until one appears, then focus
    open -a "$APP" >/dev/null 2>&1 || true
    for _ in {1..40}; do
        ID=$(aerospace list-windows --all --json | jq -r --arg app "$APP" '[.[] | select(."app-name"==$app)][0]."window-id"')
        if [[ -n "$ID" && "$ID" != null ]]; then
            aerospace focus --window-id "$ID"
            break
        fi
        sleep 0.2
    done
fi