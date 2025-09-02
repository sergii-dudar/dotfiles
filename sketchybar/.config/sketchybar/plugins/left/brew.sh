#!/usr/bin/env bash

source "$CONFIG_DIR/icons.sh"
source "$CONFIG_DIR/colors.sh"
source "$CONFIG_DIR/settings.sh"

function load_outdated() {
    OUTDATED=$(/opt/homebrew/bin/brew update &>/dev/null ; /opt/homebrew/bin/brew outdated --verbose)
    OUTDATED_COUNT=$(echo "$OUTDATED" | rg -v '^$' | wc -l | tr -d ' ')
}

function refresh() {
    load_outdated
    # echo "update oudated 2" > /tmp/logs.txt
    if [ -z "$OUTDATED" ]; then
        sketchybar -m --remove '/brew.popup\.*/' >/dev/null
        echo "cleared" > /tmp/logs.txt
        return
    fi

    # args=(--set "$NAME" icon.color="$RED")
    args=()
    if sketchybar --query "$NAME" | jq '.popup.items | length != 0' | rg -q true; then
        args+=(--remove '/brew.popup\.*/')
    fi

    COUNTER=0
    while IFS= read -r package; do
        args+=(
            --add item "$NAME".popup."$COUNTER" popup."$NAME"
            --set "$NAME".popup."$COUNTER" \
                label="${package}" \
                label.color="$ACCENT_PRIMARY" \
                background.padding_right=10 \
                background.padding_left=10 \
                background.drawing=off
        )
        COUNTER=$((COUNTER + 1))
    done <<<"$OUTDATED"

    echo "rendering" > /tmp/logs.txt
    sketchybar -m "${args[@]}" >/dev/null
    echo "rendered" > /tmp/logs.txt
}

# echo "sender: $SENDER, button: $BUTTON, modifier: $MODIFIER, scroll_delta: $SCROLL_DELTA" > /tmp/logs.txt
case "$SENDER" in
    "mouse.entered")
        popup on
        exit 0
        ;;
    "mouse.exited" | "mouse.exited.global")
        popup off
        exit 0
        ;;
    "mouse.clicked")
        case "$BUTTON" in
            "left")
                # refresh
                popup off
                "$CONFIG_DIR"/scripts/run_external_bash.sh '/opt/homebrew/bin/brew update && /opt/homebrew/bin/brew upgrade && /opt/homebrew/bin/brew cleanup && sketchybar --trigger brew_update && exit'
                ;;
            "other")
                open 'x-apple.systempreferences:com.apple.Software-Update-Settings.extension'
                ;;
                # "right")
                #     open 'x-apple.systempreferences:com.apple.Software-Update-Settings.extension'
                #     ;;
        esac
        ;;
    *)
        refresh
        ;;
esac

# COLOR=$RED
COLOR="$DEFAULT_LABEL_COLOR"

if [ -z "$OUTDATED_COUNT" ]; then
    load_outdated
    # echo "update oudated 1" > /tmp/logs.txt
fi

# echo "COUNT: $COUNT" > /tmp/logs.txt
case "$OUTDATED_COUNT" in
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
        OUTDATED_COUNT="$PACKAGES_SYNC_OK"
        ;;
esac

sketchybar --set "$NAME" \
    label="$OUTDATED_COUNT" \
    label.color="$COLOR"