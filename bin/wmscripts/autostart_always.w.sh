#!/usr/bin/env bash

#feh --bg-fill ~/wallpapers/jpg/backiee-279798-landscape.jpg &
#feh --bg-fill ~/wallpapers/png/anime/Arch-chan_to.png &
#feh --bg-fill ~/wallpapers/png/different/nord_scenary.png &

# feh --bg-fill ~/wallpapers/jpg/Wall147.jpg --bg-fill /home/serhii/wallpapers/portrait/AzZxWk0.png &
#
wm_name="${1:-}"

case "$wm_name" in
    sway)
        ;;
    hyprland)
        hyprpaper &
        ;;
esac