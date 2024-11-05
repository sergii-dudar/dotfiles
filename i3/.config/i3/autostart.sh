#!/bin/sh

#xrandr --output DP-1 --off --output DP-2 --mode 1920x1080 --pos 0x654 --rotate normal --output DP-3 --primary --mode 1920x1080 --pos 1920x0 --rotate normal --output HDMI-1 --mode 1920x1080 --pos 1920x1080 --rotate normal

# compositor
killall picom; picom -b --config ~/.config/picom/picom.conf --vsync &
#flatpak run com.borgbase.Vorta &

~/.config/polybar/launch.sh &

#bg
#nitrogen --restore &
#~/.fehbg &
#feh --bg-scale ~/wallpapers/png/wall.png &
#feh --bg-fill --randomize ~/wallpapers/png &
feh --bg-fill ~/wallpapers/jpg/backiee-279798-landscape.jpg &
#clipmenud &
#ssh-add &
#dunst &
autotiling &

#[ ! -s ~/.config/mpd/pid ] && mpd &
#/usr/libexec/polkit-gnome-authentication-agent-1 &
#/usr/lib/polkit-kde-authentication-agent-1 &

#sxhkd >/dev/null 2>&1 &
killall sxhkd; sxhkd -c ~/.config/i3/sxhkd/sxhkdrc &