#!/bin/bash

source "$HOME/.config/sketchybar/colors.sh"

POMO_HISTORY="$HOME/projects/footprints/pomodoro/.pomodoro_history"
today=$(date '+%Y-%m-%d')

output=""

if [ -f "$POMO_HISTORY" ]; then
    while IFS= read -r line || [ -n "$line" ]; do
        if [[ "$line" == "$today"* ]]; then
            if [[ "$line" == *"[WORK]"* ]]; then
                output+="🍅"
            elif [[ "$line" == *"[REST]"* ]]; then
                output+="☕️"
            fi
        fi
    done < "$POMO_HISTORY"
fi

if [ -z "$output" ]; then
    output="No Pomo"
fi

sketchybar --set pomo_history label="$output" label.color="$YELLOW"
