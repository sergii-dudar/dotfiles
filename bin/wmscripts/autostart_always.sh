#!/usr/bin/env bash

#feh --bg-fill ~/wallpapers/jpg/backiee-279798-landscape.jpg &
#feh --bg-fill ~/wallpapers/png/anime/Arch-chan_to.png &
feh --bg-fill ~/wallpapers/png/different/nord_scenary.png &
# auto lock screen
# xss-lock grabs a logind suspend inhibit lock and will use i3lock-color to lock the
# screen before suspend.
#xss-lock --transfer-sleep-lock -- i3lock --nofork


# compositor
##killall picom; picom --backend glx -b --config ~/.config/picom/picom.conf --vsync &
#killall picom; picom -b --config ~/.config/i3/picom_configurations/1.conf &

#killall sxhkd; sxhkd -c ~/.config/sxhkd/sxhkdrc &