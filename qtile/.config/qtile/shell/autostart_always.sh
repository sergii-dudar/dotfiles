#!/bin/sh

#xrandr --output DP-1 --off --output DP-2 --mode 1920x1080 --pos 0x654 --rotate normal --output DP-3 --primary --mode 1920x1080 --pos 1920x0 --rotate normal --output HDMI-1 --mode 1920x1080 --pos 1920x1080 --rotate normal

# compositor
killall picom; picom --backend glx -b --config ~/.config/picom/picom.conf --vsync &
#killall picom; picom -b --config ~/.config/i3/picom_configurations/1.conf &
#flatpak run com.borgbase.Vorta &

# be careful with autotiming if you are using tabbed or stacking can be strange, as sub tree will be diff, because it's became dymanic
# but it's ok it you will switch parent window to needed layout, and then all fine with `autotiling`.
# autotiling &

#~/.config/polybar/launch.sh &
# NetworkManager is the most popular way to manage wireless networks on Linux,
# and nm-applet is a desktop environment-independent system tray GUI for it.
nm-applet &

feh --bg-fill ~/wallpapers/jpg/backiee-279798-landscape.jpg &

# auto lock screen
# xss-lock grabs a logind suspend inhibit lock and will use i3lock-color to lock the
# screen before suspend.
#xss-lock --transfer-sleep-lock -- i3lock --nofork

killall sxhkd; sxhkd -c ~/.config/qtile/sxhkd/sxhkdrc &