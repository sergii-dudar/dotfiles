#!/usr/bin/env bash

BATTERY_INFO="$(pmset -g batt)"
PERCENTAGE=$(echo "$BATTERY_INFO" | grep -Eo "\d+%" | cut -d% -f1)
CHARGING=$(echo "$BATTERY_INFO" | grep 'AC Power')

if [ "$PERCENTAGE" = "" ]; then
    exit 0
fi

case "${PERCENTAGE}" in
    9[0-9]|100) ICON="􀛨 "
        ;;
    [6-8][0-9]) ICON="􀺸 "
        ;;
    [3-5][0-9]) ICON="􀺶 "
        ;;
    [1-2][0-9]) ICON="􀛩 "
        ;;
    *) ICON="􀛪 "
esac

if [[ "$CHARGING" != "" ]]; then
    ICON="􀢋 "
fi

sketchybar --set "$NAME" icon="$ICON" label="${PERCENTAGE}%"