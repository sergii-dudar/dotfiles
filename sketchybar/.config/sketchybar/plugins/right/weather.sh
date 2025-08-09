#!/usr/bin/env bash

. "$HOME"/dotfiles/bin/wmscripts/status-bar/shared-openweather.cached

ICON_OFFSET=3
case "$WEATHER_ICON" in
    01d)
        icon="☀️" # Clear sky (day)
        ;;
    01n)
        icon="🌛" # Clear sky (night) "🌙" "🌕"
        ;;
    02d)
        icon="🌤️" # Few clouds (day) "🌤" 🌤️"
        ;;
    02n)
        icon="☁️" # Few clouds (night) "🌥" "☁️"
        ;;
    03d)
        icon="⛅" # Scattered clouds
        ICON_OFFSET=4
        ;;
    03n)
        icon="☁️" # Scattered clouds
        ;;
    04d)
        icon="⛅" # Broken clouds "🌥" "☁️"
        ICON_OFFSET=4
        ;;
    04n)
        icon="☁️" # Broken clouds "🌥" "☁️"
        ;;
    09d)
        icon="🌧️" # Shower rain (day) "🌧️" "🌧"
        ICON_OFFSET=2
        ;;
    09n)
        icon="🌧️" # Shower rain (night) "🌧️" "🌧"
        ICON_OFFSET=2
        ;;
    10d)
        icon="🌦️" # Rain (day) "⛈" "🌦"
        ;;
    10n)
        icon="⛈️" # Rain (night) "⛈" "🌧"
        ;;
    11d)
        icon="🌩️" # Thunderstorm (day) "🌩" "⛈"
        ;;
    11n)
        icon="🌩️" # Thunderstorm (night)
        ;;
    13d)
        icon="🌨️" # Snow (day) "🌨️" "❄️"
        ;;
    13n)
        icon="🌨️" # Snow (night) "🌨️" "❄️"
        ;;
    50d)
        icon="🌫️" # Mist (day)
        ;;
    50n)
        icon="🌫️" # Mist (night)
        ;;
    *)
        icon="✨"   # Unknown condition
        ;;
esac


sketchybar -m \
    --set "$NAME" icon="$icon" \
    "$NAME" label="$TEMPERATURE"°C \
    icon.y_offset="$ICON_OFFSET"
