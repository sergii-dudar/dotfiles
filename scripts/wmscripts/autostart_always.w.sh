#!/usr/bin/env bash

#feh --bg-fill ~/wallpapers/jpg/backiee-279798-landscape.jpg &
#feh --bg-fill ~/wallpapers/png/anime/Arch-chan_to.png &
#feh --bg-fill ~/wallpapers/png/different/nord_scenary.png &

# feh --bg-fill ~/wallpapers/jpg/Wall147.jpg --bg-fill /home/serhii/wallpapers/portrait/AzZxWk0.png &
#
wm_name="${1:-}"

~/dotfiles/scripts/monitor-exists.sh HDMI-A-1 && swaybg -o HDMI-A-1 -i "$(cat ~/dotfiles/scripts/wallpapers/selected/HDMI-A-1.txt)" -m fill &
~/dotfiles/scripts/monitor-exists.sh HDMI-A-2 && swaybg -o HDMI-A-2 -i "$(cat ~/dotfiles/scripts/wallpapers/selected/HDMI-A-2.txt)" -m fill &
~/dotfiles/scripts/monitor-exists.sh HDMI-A-3 && swaybg -o HDMI-A-3 -i "$(cat ~/dotfiles/scripts/wallpapers/selected/HDMI-A-3.txt)" -m fill &

case "$wm_name" in
    dwl)
        # swaybg -o HDMI-A-1 -i ~/wallpapers/jpg/Wall147.jpg -m fill &
        # swaybg -o HDMI-A-2 -i ~/wallpapers/jpg/Wall147.jpg -m fill &
        # swaybg -o HDMI-A-3 -i ~/wallpapers/portrait/AzZxWk0.png -m fill &
        ;;
    sway)
        ;;
    hyprland)
        # hyprpaper &
        ;;
esac
