# shellcheck shell=bash

run_segment() {
    . "$HOME"/dotfiles/bin/wmscripts/status-bar/shared-openweather.cached
    get_weather
}
