#!/usr/bin/env bash


# be careful with autotiming if you are using tabbed or stacking can be strange, as sub tree will be diff, because it's became dymanic
# but it's ok it you will switch parent window to needed layout, and then all fine with `autotiling`.
# autotiling &

~/.config/polybar/i3/launch.sh &
feh --bg-fill ~/wallpapers/jpg/backiee-279798-landscape.jpg &
killall sxhkd; sxhkd -c ~/.config/sxhkd/i3/sxhkdrc &