#!/usr/bin/env bash

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
    ICON=" "
    VOLUME=100
elif [ "$MUTED" != "false" ]; then
    ICON="􀊣 "
    VOLUME=0
else
    case ${VOLUME} in
        100) ICON=" " ;;
        9[0-9]) ICON=" " ;;
        8[0-9]) ICON=" " ;;
        7[0-9]) ICON=" " ;;
        6[0-9]) ICON=" " ;;
        5[0-9]) ICON=" " ;;
        4[0-9]) ICON=" " ;;
        3[0-9]) ICON=" " ;;
        2[0-9]) ICON=" " ;;
        1[0-9]) ICON=" " ;;
        [0-9]) ICON=" " ;;
        *) ICON=" " ;;
    esac
fi

sketchybar -m \
    --set "$NAME" icon="$ICON" \
    --set "$NAME" label="$VOLUME%"