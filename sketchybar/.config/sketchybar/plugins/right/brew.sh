#!/usr/bin/env bash

source "$CONFIG_DIR/colors.sh"

if [[ "$SENDER" == "mouse."* ]]; then
    # echo "sender: $SENDER, button: $BUTTON, modifier: $MODIFIER, scroll_delta: $SCROLL_DELTA" > /tmp/logs.txt
    case "$SENDER" in
        "mouse.clicked")
            case "$BUTTON" in
                "left")
                    "$CONFIG_DIR"/scripts/run_external_bash.sh '/opt/homebrew/bin/brew update && /opt/homebrew/bin/brew upgrade && exit'
                    exit 0
                    ;;
                "right")
                    open 'x-apple.systempreferences:com.apple.Software-Update-Settings.extension'
                    exit 0
                    ;;
            esac
            ;;
    esac
fi

COUNT=$(brew outdated | wc -l | tr -d ' ')
COLOR=$RED

case "$COUNT" in
    [3-5][0-9])
        COLOR=$ORANGE
        ;;
    [1-2][0-9])
        COLOR=$YELLOW
        ;;
    [1-9])
        COLOR=$WHITE
        ;;
    0)
        COLOR=$GREEN
        COUNT=􀆅
        ;;
esac

sketchybar --set $NAME \
    label=$COUNT
#    icon.color=$COLOR