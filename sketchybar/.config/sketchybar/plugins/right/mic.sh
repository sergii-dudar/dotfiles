#!/usr/bin/env bash

VOLUME=$(osascript -e 'input volume of (get volume settings)')

if [[ "$SENDER" == "mouse."* ]]; then
    # echo "sender: $SENDER, button: $BUTTON, modifier: $MODIFIER, scroll_delta: $SCROLL_DELTA" > /tmp/logs.txt
    case "$SENDER" in
        "mouse.clicked")
            case "$BUTTON" in
                "left")

                    # Check if the microphone is currently muted (volume is 0)
                    if [ "$VOLUME" -eq 0 ]; then
                        # Unmute the microphone (set volume to 100)
                        osascript -e "set volume input volume 90"
                        osascript -e "display notification \"Microphone is ON\" with title \"🎤 Unmuted\" sound name \"Pop\""
                    else
                        # Mute the microphone (set volume to 0)
                        osascript -e "set volume input volume 0"
                        osascript -e "display notification \"Microphone is OFF\" with title \"🎤 Muted\" sound name \"Pop\""
                    fi
                    ;;
                "right")
                    open 'x-apple.systempreferences:com.apple.Sound-Settings.extension?input'
                    ;;
            esac
            ;;
        "mouse.scrolled")
            if [ "$SCROLL_DELTA" -gt 0 ]; then
                osascript -e "set volume input volume ((input volume of (get volume settings)) + 5)"
            else
                osascript -e "set volume input volume ((input volume of (get volume settings)) - 5)"
            fi
            ;;
    esac
fi


INPUT_NAME=$(SwitchAudioSource -t input -c)

case $INPUT_NAME in
    'MacBook Pro Microphone')
        INPUT_NAME="Mic"
        ;;
    'USB PnP Audio Device')
        INPUT_NAME="USB"
        ;;
    'HD Pro Webcam C920')
        INPUT_NAME="USB"
        ;;
esac

ICON="􀊰 "
HIGHLIGH=off
if [ "$VOLUME" -eq 0 ]; then
    ICON=􀊲
    HIGHLIGH=on
fi

sketchybar --set "$NAME" \
    label="$VOLUME%" \
    icon="$ICON" \
    icon.highlight=$HIGHLIGH