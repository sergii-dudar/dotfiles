#!/usr/bin/env bash

source "$CONFIG_DIR/icons.sh"
source "$CONFIG_DIR/colors.sh"
source "$CONFIG_DIR/settings.sh"

function load_scratchpad_info() {
    SCRATCHPAD_APPS=$(aerospace list-windows --workspace "NSP" 2>/dev/null)
    SCRATCHPAD_COUNT=$(echo "$SCRATCHPAD_APPS" | wc -l | awk '{print $1}')
}

function refresh() {
    load_scratchpad_info

    if [ -z "$SCRATCHPAD_APPS" ]; then
        return
    fi

    args=(--remove '/scratchpad.popup\.*/' )

    local counter=0
    while IFS='|' read -r id label rest; do
        # trim leading/trailing spaces
        label="${label#"${label%%[![:space:]]*}"}"
        label="${label%"${label##*[![:space:]]}"}"

        icon=$("$CONFIG_DIR"/scripts/icon_map_fn.sh "$label")
        icon_font="$ICON_APP_FONT"

        if [ "$icon" = ":default:" ]; then
            case "$label" in
                "Google Chat")
                    icon=":messages:" # :messages: :messenger:
                    ;;
                "Monkeytype")
                    icon=" "
                    icon_font="$ICON_FONT"
                    ;;
                "VimHero")
                    icon=":neovim:"
                    ;;
            esac
        fi

        args+=(\
                --add item "$NAME".popup."$counter" popup."$NAME" \
                --set "$NAME".popup."$counter" \
                label="$label" \
                icon.font="$icon_font" \
                icon="$icon" \
                icon.padding_right=6 \
                label.color="$ACCENT_PRIMARY" \
                background.padding_right=10 \
                background.padding_left=10 \
                background.drawing=off\
            )
        counter=$((counter + 1))

    done <<< "$SCRATCHPAD_APPS"

    sketchybar -m "${args[@]}" >/dev/null
}

# echo "sender: $SENDER, button: $BUTTON, modifier: $MODIFIER, scroll_delta: $SCROLL_DELTA" > /tmp/logs.txt
case "$SENDER" in
    "mouse.entered")
        popup on
        exit 0
        ;;
    "mouse.exited" | "mouse.exited.global" | "mouse.clicked")
        popup off
        exit 0
        ;;
    *)
        refresh
        ;;
esac

if [ -z "$SCRATCHPAD_COUNT" ]; then
    load_scratchpad_info
fi

sketchybar --set "$NAME" \
    label="$SCRATCHPAD_COUNT"