# shellcheck shell=bash

run_segment() {
    . "$HOME"/dotfiles/scripts/wmscripts/status-bar/shared-openweather.cached
    get_weather
}
