#!/usr/bin/env bash

source "$CONFIG_DIR/icons.sh"

if [[ "$SENDER" == "mouse."* ]]; then
    # echo "sender: $SENDER, button: $BUTTON, modifier: $MODIFIER, scroll_delta: $SCROLL_DELTA" > /tmp/logs.txt
    case "$SENDER" in
        "mouse.clicked")
            case "$BUTTON" in
                "left")
                    osascript -e "set isMuted to output muted of (get volume settings)
                        if isMuted then
                            set volume without output muted
                            display notification \"Sound is ON\" with title \"🔊 Unmuted\" sound name \"Pop\"
                        else
                            set volume with output muted
                            display notification \"Sound is OFF\" with title \"🔇 Muted\" sound name \"Pop\"
                    end if"
                    ;;
                "right")
                    open 'x-apple.systempreferences:com.apple.Sound-Settings.extension'
                    ;;
            esac
            ;;
        "mouse.scrolled")
            if [ "$SCROLL_DELTA" -gt 0 ]; then
                osascript -e "set volume output volume ((output volume of (get volume settings)) + 5) --100%"
            else
                osascript -e "set volume output volume ((output volume of (get volume settings)) - 5) --100%"
            fi
            ;;
    esac
fi

VOLUME=$(osascript -e "output volume of (get volume settings)")
MUTED=$(osascript -e "output muted of (get volume settings)")

if [ "$MUTED" = "missing value" ]; then
    ICON="$VOLUME_100"
    VOLUME=100
elif [ "$MUTED" != "false" ]; then
    ICON="$VOLUME_0"
    VOLUME=0
else
    case ${VOLUME} in
        100) ICON="$VOLUME_100" ;;
        9[0-9]) ICON="$VOLUME_100" ;;
        8[0-9]) ICON="$VOLUME_100" ;;
        7[0-9]) ICON="$VOLUME_100" ;;
        6[0-9]) ICON="$VOLUME_100" ;;
        5[0-9]) ICON="$VOLUME_100" ;;
        4[0-9]) ICON="$VOLUME_100" ;;
        3[0-9]) ICON="$VOLUME_100" ;;
        2[0-9]) ICON="$VOLUME_66" ;;
        1[0-9]) ICON="$VOLUME_66" ;;
        [0-9]) ICON="$VOLUME_33" ;;
        *) ICON="$VOLUME_33" ;;
    esac
fi

sketchybar -m \
    --set "$NAME" icon="$ICON" \
    --set "$NAME" label="$VOLUME%"