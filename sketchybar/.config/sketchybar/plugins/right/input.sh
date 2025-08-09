#!/usr/bin/env bash

source "$CONFIG_DIR/icons.sh"
SOURCE=$(defaults read ~/Library/Preferences/com.apple.HIToolbox.plist AppleCurrentKeyboardLayoutInputSourceID)

case $SOURCE in
    'com.apple.keylayout.ABC')
        ICON="$INPUT_FLAG_ABC"
        LABEL="US"
        ;;
    'com.apple.keylayout.Ukrainian-PC')
        ICON="$INPUT_FLAG_UA"
        LABEL="UA"
        ;;
esac

sketchybar --set "$NAME" icon="$ICON" label="$LABEL"