. "$HOME"/dotfiles/bin/wmscripts/status-bar/sb-weather

# weather_icon=02d
# temperature=-2

function get_emoji() {
    case $1 in
        01d) icon=" "  ;; # Clear sky (day)
        01n) icon="" ;; # Clear sky (night) "🌙" "🌕"
        02d) icon="🌤" ;; # Few clouds (day) "🌤" 🌤️"
        02n) icon="🌥" ;; # Few clouds (night) "🌥" "☁️"
        03d) icon="🌥" ;; # Scattered clouds
        03n) icon="🌥"  ;; # Scattered clouds
        04d) icon="🌥" ;; # # Broken clouds "🌥" "☁️"
        04n) icon="🌥" ;; # Broken clouds "🌥" "☁️"
        09d) icon="🌧" ;; # Shower rain (day) "🌧️" "🌧"
        09n) icon="🌧" ;; # Shower rain (night) "🌧️" "🌧"
        10d) icon="🌦" ;; # Rain (day) "⛈" "🌦"
        10n) icon="⛈"  ;; # Rain (night) "⛈" "🌧"
        11d) icon="🌩" ;; # Thunderstorm (day) "🌩" "⛈"
        11n) icon="🌩" ;; # Thunderstorm (night)
        13d) icon="󰼶 " ;; # Snow (day) "🌨️" "❄️"
        13n) icon="󰼶 " ;; # Snow (night) "🌨️" "❄️"
        50d) icon="🌫" ;; # Mist (day)
        50n) icon="🌫" ;; # Mist (night)
        *) icon=" "   ;; # Unknown condition
    esac

    echo "$icon"
}

run_segment() {
    echo "$(get_emoji $weather_icon) $temperature"°C
	return 0
}