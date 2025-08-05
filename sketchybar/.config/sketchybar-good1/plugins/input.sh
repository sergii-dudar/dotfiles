#!/bin/bash

SOURCE=$(defaults read ~/Library/Preferences/com.apple.HIToolbox.plist AppleCurrentKeyboardLayoutInputSourceID)

case $SOURCE in
'com.apple.keylayout.ABC')
    LABEL=🇺🇸
    ;;
'com.apple.keylayout.Ukrainian-PC')
    LABEL=🇺🇦
    ;;
'com.apple.keylayout.Russian')
    LABEL=🇷🇺
    ;;
esac

sketchybar --set $NAME label=$LABEL
