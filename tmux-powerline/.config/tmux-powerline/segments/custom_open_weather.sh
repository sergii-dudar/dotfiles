# shellcheck shell=bash

# How often to update the weather in seconds.
export TMUX_POWERLINE_SEG_OPEN_WEATHER_UPDATE_PERIOD="1800" # once 30 min

run_segment() {
    tmp_file="${TMUX_POWERLINE_DIR_TEMPORARY:-/tmp}/temp_tmux_open_weather_file.txt"
    local weather_text
    weather_text=$(__openweather)

    if [ -n "$weather_text" ]; then
        echo "$weather_text"
    fi
}

__openweather() {
    if [ -f "$tmp_file" ]; then
        if shell_is_osx || shell_is_bsd; then
            last_update=$(stat -f "%m" "${tmp_file}")
        elif shell_is_linux; then
            last_update=$(stat -c "%Y" "${tmp_file}")
        fi
        time_now=$(date +%s)

        up_to_date=$(echo "(${time_now}-${last_update}) < ${TMUX_POWERLINE_SEG_OPEN_WEATHER_UPDATE_PERIOD}" | bc)
        if [ "$up_to_date" -eq 1 ]; then
            __read_tmp_file
        fi
    fi

    . "$HOME"/dotfiles/bin/wmscripts/status-bar/sb-weather
    # weather_icon=02d
    # temperature=-2

    weather_test_result="$(get_emoji $weather_icon) $temperature"°C
    # echo "write info to temp file"
    echo "$weather_test_result" | tee "${tmp_file}"
}

__read_tmp_file() {
    if [ ! -f "$tmp_file" ]; then
        return
    fi
    cat "${tmp_file}"
    exit
}

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

# run_segment