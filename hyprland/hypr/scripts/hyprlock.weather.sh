#!/usr/bin/env bash

. ~/dotfiles/scripts/wmscripts/status-bar/shared-openweather.cached

function of_font() {
    # echo "<span size=\"$2\">$1</span>"
    echo "$1"
    # echo "<span font=\"$2\">$1</span>"
}

function get_emoji() {
    case $1 in
        01d) icon=$(of_font "☀️" 18)  ;; # Clear sky (day)
            # 01d) icon=$(of_font "☀️" 13)  ;; # Clear sky (day)
        01n) icon=$(of_font "🌛" 17) ;; # Clear sky (night) "🌙" "🌕"
        02d) icon=$(of_font "🌤️" 19) ;; # Few clouds (day) "🌤" 🌤️"
        02n) icon=$(of_font "☁️" 20)  ;; # Few clouds (night) "🌥" "☁️"

        03d) icon=$(of_font "⛅" 20) ;; # Scattered clouds
        03n) icon=$(of_font "☁️" 19)  ;; # Scattered clouds
            # 04d) icon=$(of_font "🌥" 15)  ;; # # Broken clouds "🌥" "☁️"
        04d) icon=$(of_font "⛅" 20)  ;; # # Broken clouds "🌥" "☁️"
        04n) icon=$(of_font "☁️" 20)  ;; # Broken clouds "🌥" "☁️"
        09d) icon=$(of_font "🌧️" 18) ;; # Shower rain (day) "🌧️" "🌧"
        09n) icon=$(of_font "🌧️" 18) ;; # Shower rain (night) "🌧️" "🌧"

        10d) icon=$(of_font "🌦️" 19)  ;; # Rain (day) "⛈" "🌦"
        10n) icon=$(of_font "⛈️" 18)  ;; # Rain (night) "⛈" "🌧"
        11d) icon=$(of_font "🌩️" 18) ;; # Thunderstorm (day) "🌩" "⛈"
        11n) icon=$(of_font "🌩️" 18) ;; # Thunderstorm (night)
        13d) icon=$(of_font "🌨️" 18)  ;; # Snow (day) "🌨️" "❄️"
        13n) icon=$(of_font "🌨️" 18)  ;; # Snow (night) "🌨️" "❄️"
        50d) icon=$(of_font "🌫️" 16) ;; # Mist (day)
        50n) icon=$(of_font "🌫️" 16) ;; # Mist (night)
        *) icon=$(of_font "✨" 17)   ;; # Unknown condition
    esac

    echo "$icon"
}

echo "$(get_emoji $WEATHER_ICON) ${TEMPERATURE}°C"

# debug all emojis in xmobar
# echo -n "$(get_emoji "01d")|$(get_emoji "01n")|$(get_emoji "02d")|$(get_emoji "02n")|$(get_emoji "03d")|$(get_emoji "03n")|"
# echo -n "$(get_emoji "04d")|$(get_emoji "04n")|$(get_emoji "09d")|$(get_emoji "09n")|$(get_emoji "10d")|$(get_emoji "10n")|"
# echo -n "$(get_emoji "11d")|$(get_emoji "11n")|$(get_emoji "13d")|$(get_emoji "13n")|$(get_emoji "50d")|$(get_emoji "50n")|"
# echo "$(get_emoji "555")"
