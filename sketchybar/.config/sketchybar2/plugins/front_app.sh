#!/usr/bin/env bash

# Source colors and icons for consistent theming
source "$CONFIG_DIR/colors.sh"
source "$CONFIG_DIR/icons.sh"

# Get the current application
if [ "$SENDER" = "front_app_switched" ]; then
    APP_NAME="$INFO"
else
    APP_NAME=$(yabai -m query --windows --window | jq -r '.app' 2>/dev/null || echo "$(osascript -e 'tell application "System Events" to get name of first application process whose frontmost is true')")
fi

# Icon mapping for common applications
case $APP_NAME in
    "Cursor")
        ICON="󰨞"
        ;;
    "Visual Studio Code")
        ICON="󰨞"
        ;;
    "Slack")
        ICON="󰒱"
        ;;
    "Discord")
        ICON="󰙯"
        ;;
    "Google Chrome")
        ICON="󰊯"
        ;;
    "Safari")
        ICON="󰀹"
        ;;
    "Firefox")
        ICON="󰈹"
        ;;
    "Terminal")
        ICON="󰆍"
        ;;
    "iTerm2")
        ICON="󰆍"
        ;;
    "Finder")
        ICON="󰀶"
        ;;
    "Spotify")
        ICON="󰓇"
        ;;
    "Music")
        ICON="󰎆"
        ;;
    "Mail")
        ICON="󰇮"
        ;;
    "Messages")
        ICON="󰍦"
        ;;
    "FaceTime")
        ICON="󰍫"
        ;;
    "Zoom")
        ICON="󰊶"
        ;;
    "Notion")
        ICON="󰈭"
        ;;
    "Obsidian")
        ICON="󰈭"
        ;;
    "Xcode")
        ICON="󰙳"
        ;;
    "Docker")
        ICON="󰡨"
        ;;
    "Figma")
        ICON="󰤼"
        ;;
    "Sketch")
        ICON="󰤼"
        ;;
    "Photoshop")
        ICON="󰤿"
        ;;
    "System Preferences")
        ICON="󰒓"
        ;;
    "System Settings")
        ICON="󰒓"
        ;;
    "Activity Monitor")
        ICON="󰖚"
        ;;
    *)
        # Default icon for unknown applications
        ICON="󰄛"
        ;;
esac

# Update the front_app item
sketchybar --set "$NAME" icon="$ICON" \
                        icon.color=$ACCENT_QUATERNARY \
                        label="$APP_NAME" \
                        label.color=$WHITE
