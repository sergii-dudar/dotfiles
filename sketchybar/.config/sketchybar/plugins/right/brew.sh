#!/usr/bin/env bash

source "$CONFIG_DIR/icons.sh"
source "$CONFIG_DIR/colors.sh"

if [[ "$SENDER" == "mouse."* ]]; then
    # echo "sender: $SENDER, button: $BUTTON, modifier: $MODIFIER, scroll_delta: $SCROLL_DELTA" > /tmp/logs.txt
    case "$SENDER" in
        "mouse.clicked")
            case "$BUTTON" in
                "left")
                    "$CONFIG_DIR"/scripts/run_external_bash.sh '/opt/homebrew/bin/brew update && /opt/homebrew/bin/brew upgrade && sketchybar --trigger brew_update && exit'
                    ;;
                "other")
                    open 'x-apple.systempreferences:com.apple.Software-Update-Settings.extension'
                    ;;
                    # "right")
                    #     open 'x-apple.systempreferences:com.apple.Software-Update-Settings.extension'
                    #     ;;
            esac
            ;;
    esac
fi

COUNT=$(brew outdated | wc -l | tr -d ' ')
# COLOR=$RED
COLOR="$DEFAULT_LABEL_COLOR"

# echo "COUNT: $COUNT" > /tmp/logs.txt
case "$COUNT" in
    [3-5][0-9])
        COLOR=$ORANGE
        ;;
    [1-2][0-9])
        COLOR=$YELLOW
        ;;
    [1-9])
        #COLOR=$WHITE
        ;;
    0)
        COUNT="$PACKAGES_SYNC_OK"
        ;;
esac

sketchybar --set "$NAME" \
    label="$COUNT" \
    label.color="$COLOR"