#!/usr/bin/env bash

BATTERY_INFO="$(pmset -g batt)"
PERCENTAGE=$(echo "$BATTERY_INFO" | grep -Eo "\d+%" | cut -d% -f1)
CHARGING=$(echo "$BATTERY_INFO" | grep 'AC Power')

if [ "$PERCENTAGE" = "" ]; then
    exit 0
fi

source "$CONFIG_DIR/icons.sh"

case "${PERCENTAGE}" in
    9[0-9]|100) ICON="$BATTERY_100" ;;
    [6-8][0-9]) ICON="$BATTERY_75" ;;
    [3-5][0-9]) ICON="$BATTERY_50" ;;
    [1-2][0-9]) ICON="$BATTERY_25" ;;
    *) ICON="$BATTERY_0"
esac

if [[ "$CHARGING" != "" ]]; then
    ICON="$BATTERY_CHARGING"
fi

sketchybar --set "$NAME" icon="$ICON" label="${PERCENTAGE}%"