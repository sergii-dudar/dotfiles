#!/usr/bin/env bash

if [[ "$SENDER" == "mouse."* ]]; then
    case "$SENDER" in
        "mouse.clicked")
            # echo "sender: $SENDER, button: $BUTTON, modifier: $MODIFIER, scroll_delta: $SCROLL_DELTA" > /tmp/logs.txt
            case "$BUTTON" in
                "left")
                    "$CONFIG_DIR"/scripts/run_external_bash.sh '/opt/homebrew/bin/gdu-go ~ && exit'
                    ;;
                "right")
                    "$CONFIG_DIR"/scripts/run_external_bash.sh "$HOME/.cargo/bin/yazi && exit"
                    ;;
            esac
            ;;
    esac
fi

LABEL=$(df -H | grep -E '^(/dev/disk3s5).' | awk '{ printf ("%s\n", $5) }')
sketchybar --set "$NAME" label="$LABEL"