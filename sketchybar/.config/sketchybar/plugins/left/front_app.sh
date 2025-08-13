#!/usr/bin/env bash

if [ "$SENDER" = "front_app_switched" ]; then
    ICON=$("$CONFIG_DIR"/scripts/icon_map_fn.sh "$INFO")

    # echo "$ICON" > /tmp/logs.txt
    ICON_Y_OFFSET=1
    ICON_FONT_SIZE=17.0
    case "$ICON" in
        :ghostty:)
            ICON_COLOR=0xff4E49EE
            ICON_FONT_SIZE=20
            ;;
        :idea:) ICON_COLOR=0xff8caaee ;;
        :brave_browser:)
            ICON_COLOR=0xfff38ba8
            ICON_FONT_SIZE=18
            ;;
        :microsoft_teams:) ICON_COLOR=0xff81A1C1 ;;
        :gear:) ICON_COLOR=0xff3071db ;;
        :kitty:) ICON_COLOR=0xff89dceb ;;
        :wezterm:) ICON_COLOR=0xff4E49EE ;;
        :postman:) ICON_COLOR=0xffa6d189 ;;
        :music:)
            ICON_COLOR=0xfff38ba8
            ICON_FONT_SIZE=18
            ;;
        :iterm:)
            ICON_COLOR=0xff4E49EE
            ICON_FONT_SIZE=19
            ;;
        *) ICON_COLOR=0xff94928F ;;
    esac

    sketchybar --set "$NAME" \
        label="$INFO" \
        icon="$ICON" \
        icon.color="$ICON_COLOR" \
        icon.y_offset="$ICON_Y_OFFSET" \
        icon.font.size="$ICON_FONT_SIZE"


fi