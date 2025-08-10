#!/usr/bin/env bash

source "$CONFIG_DIR/icons.sh"
source "$CONFIG_DIR/colors.sh"

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
OUTPUT_NAME=$(SwitchAudioSource -t output -c)

case $OUTPUT_NAME in
    'MacBook'*)
        DEVICE_ICON="$VOL_DEVICE_INTERNAL"
        ;;
    'LG'*)
        DEVICE_ICON="$VOL_DEVICE_EX_MONITOR"
        ;;
    'External Headphones')
        DEVICE_ICON="$VOL_DEVICE_EX_HEADPHONES"
        ;;
        # 'USB')
        #     DEVICE_ICON="$VOL_DEVICE_EX_DAC"
        #     ;;
    *) DEVICE_ICON="$QUESTION" ;;
esac


if [ "$MUTED" = "missing value" ]; then
    ICON="$VOLUME_100"
    VOLUME=100
    ICON_COLOR="$VOLUME_ICON_COLOR"
elif [ "$MUTED" != "false" ]; then
    ICON="$VOLUME_0"
    VOLUME=0
    ICON_COLOR="$RED"
else
    case ${VOLUME} in
        100) ICON="$VOLUME_100" ;;
        9[0-9]) ICON="$VOLUME_100" ;;
        8[0-9]) ICON="$VOLUME_100" ;;
        7[0-9]) ICON="$VOLUME_100" ;;
        6[0-9]) ICON="$VOLUME_100" ;;
        5[0-9]) ICON="$VOLUME_100" ;;
        4[0-9]) ICON="$VOLUME_100" ;;
        3[0-9]) ICON="$VOLUME_100" ;;
        2[0-9]) ICON="$VOLUME_66" ;;
        1[0-9]) ICON="$VOLUME_66" ;;
        [0-9]) ICON="$VOLUME_33" ;;
        *) ICON="$VOLUME_33" ;;
    esac
    ICON_COLOR="$VOLUME_ICON_COLOR"
fi

VAL_FORMATTED=$(printf "%02d" "$VOLUME")
sketchybar \
    --set "$NAME" label="$VAL_FORMATTED% $DEVICE_ICON" \
    icon="$ICON" \
    icon.color="$ICON_COLOR"