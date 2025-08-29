#!/usr/bin/env bash

source "$CONFIG_DIR/icons.sh"
source "$CONFIG_DIR/colors.sh"
source "$CONFIG_DIR/settings.sh"

# COUNT="$(aerospace list-windows --workspace "NSP" 2>/dev/null | wc -l | awk '{print $1}')"
function load_scratchpad_info() {
    SCRATCHPAD_APPS=$(aerospace list-windows --workspace "NSP" 2>/dev/null)
    SCRATCHPAD_COUNT=$($SCRATCHPAD_APPS | wc -l | awk '{print $1}')
}

function refresh() {
    load_scratchpad_info

    echo "update oudated 2" > /tmp/logs.txt

    if [ -z "$SCRATCHPAD_APPS" ]; then
        return
    fi


    args=(--set "$NAME")
    args+=(--remove '/scratchpad.popup\.*/')

    COUNTER=0
    while IFS='|' read -r id name rest; do
        # trim leading/trailing spaces
        name="${name#"${name%%[![:space:]]*}"}"
        name="${name%"${name##*[![:space:]]}"}"

        icon=$("$CONFIG_DIR"/scripts/icon_map_fn.sh "$name")

        # iTerm
        # Google Chat
        # Sublime Text
        # Monkeytype
        # VimHero

        # case $name in
        #     "Google Chat") icon="💬" ;;
        #     "Monkey Type") icon="🐒" ;;
        #     "Terminal")    icon="💻" ;;
        #     *)             icon="❓" ;;
        # esac

        args+=(
            --add item "$NAME".popup."$COUNTER" popup."$NAME"
            --set "$NAME".popup."$COUNTER" \
                label="$name" \
                icon="$icon" \
                label.color="$ACCENT_PRIMARY" \
                background.padding_right=10 \
                background.padding_left=10 \
                background.drawing=off
        )
        COUNTER=$((COUNTER + 1))

        # echo "$icon $name"
    done <<< "$SCRATCHPAD_APPS"

    sketchybar -m "${args[@]}" >/dev/null
}

# echo "sender: $SENDER, button: $BUTTON, modifier: $MODIFIER, scroll_delta: $SCROLL_DELTA" > /tmp/logs.txt
case "$SENDER" in
    "routine" | "forced" | "scratchpad_update")
        refresh
        echo "refreshed" > /tmp/logs.txt
        ;;
    "mouse.entered")
        popup on
        echo "focused" > /tmp/logs.txt
        exit 0
        ;;
    "mouse.exited" | "mouse.exited.global")
        popup off
        echo "focuse exited" > /tmp/logs.txt
        exit 0
        ;;
    "mouse.clicked")
        popup off
        echo "clicked" > /tmp/logs.txt
        exit 0
        ;;
esac



if [ -z "$SCRATCHPAD_COUNT" ]; then
    load_scratchpad_info
    echo "update oudated 1" > /tmp/logs.txt
fi

# COUNT="$(aerospace list-windows --workspace "NSP" 2>/dev/null | wc -l | awk '{print $1}')"

sketchybar --set "$NAME" \
    label="$SCRATCHPAD_COUNT"