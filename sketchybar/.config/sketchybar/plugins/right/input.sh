#!/usr/bin/env bash

SOURCE=$(defaults read ~/Library/Preferences/com.apple.HIToolbox.plist AppleCurrentKeyboardLayoutInputSourceID)

case $SOURCE in
    'com.apple.keylayout.ABC')
        ICON="🇺🇲"
        LABEL="US"
        ;;
    'com.apple.keylayout.Ukrainian-PC')
        ICON="🇺🇦"
        LABEL="UA"
        ;;
esac

sketchybar --set "$NAME" icon="$ICON" label="$LABEL"
