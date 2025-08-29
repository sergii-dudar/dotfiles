#!/usr/bin/env bash

CONFIG_DIR="$HOME/dotfiles/sketchybar/.config/sketchybar/"
NAME="scratchpad"

source "$CONFIG_DIR/icons.sh"
source "$CONFIG_DIR/colors.sh"
source "$CONFIG_DIR/settings.sh"

INPUT=$'1234  | Google Chat   | Something
5678 | Monkey Type | Some2
9327 | Terminal | Test3
2532 | Ghostty | 4534'

# # turn into a loop
# while IFS='|' read -r id name rest; do
#     # trim leading/trailing spaces
#     name="${name#"${name%%[![:space:]]*}"}"
#     name="${name%"${name##*[![:space:]]}"}"
#
#     # Example: attach an icon based on name
#     case $name in
#         "Google Chat") icon="💬" ;;
#         "Monkey Type") icon="🐒" ;;
#         "Terminal")    icon="💻" ;;
#         *)             icon="❓" ;;
#     esac
#
#     echo "$icon $name"
# done <<< "$INPUT"



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
done <<< "$INPUT"

# sketchybar -m "${args[@]}" >/dev/null
echo "${args[@]}"