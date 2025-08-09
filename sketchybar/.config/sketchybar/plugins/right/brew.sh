#!/usr/bin/env bash

source "$CONFIG_DIR/icons.sh"

if [[ "$SENDER" == "mouse."* ]]; then
    # echo "sender: $SENDER, button: $BUTTON, modifier: $MODIFIER, scroll_delta: $SCROLL_DELTA" > /tmp/logs.txt
    case "$SENDER" in
        "mouse.clicked")
            case "$BUTTON" in
                "left")
                    "$CONFIG_DIR"/scripts/run_external_bash.sh '/opt/homebrew/bin/brew update && /opt/homebrew/bin/brew upgrade && exit'
                    ;;
                "right")
                    open 'x-apple.systempreferences:com.apple.Software-Update-Settings.extension'
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
        COUNT="$PACKAGES_SYNC_OK"
        ;;
esac

sketchybar --set $NAME \
    label=$COUNT
#    icon.color=$COLOR