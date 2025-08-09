#!/usr/bin/env bash

# Get CPU usage percentage
CPU_USAGE=$(top -l 1 -n 0 | grep "CPU usage" | awk '{print $3}' | sed 's/%//')

# Handle empty CPU usage
if [[ -z "$CPU_USAGE" ]]; then
    CPU_USAGE=0
fi

# Remove decimal point for comparison
CPU_INT=${CPU_USAGE%.*}

## Set icon and color based on CPU usage
# source "$CONFIG_DIR/colors.sh"
# if [[ $CPU_INT -le 25 ]]; then
#     COLOR=$ACCENT_SECONDARY  # Green
# elif [[ $CPU_INT -le 50 ]]; then
#     COLOR=$ACCENT_PRIMARY  # Blue
# elif [[ $CPU_INT -le 75 ]]; then
#     COLOR=$ACCENT_TERTIARY  # Orange
# else
#     COLOR=$RED  # Red
# fi

sketchybar --set "$NAME" label="$CPU_INT%"