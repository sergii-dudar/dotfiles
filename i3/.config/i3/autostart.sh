#!/bin/sh

#xrandr --output DP-1 --off --output DP-2 --mode 1920x1080 --pos 0x654 --rotate normal --output DP-3 --primary --mode 1920x1080 --pos 1920x0 --rotate normal --output HDMI-1 --mode 1920x1080 --pos 1920x1080 --rotate normal

# compositor
killall picom; picom -b --config ~/.config/picom/picom.conf --vsync &
#killall picom; picom -b --config ~/.config/i3/picom_configurations/1.conf &
#flatpak run com.borgbase.Vorta &

~/.config/polybar/launch.sh &

#bg
#nitrogen --restore &
#~/.fehbg &
#feh --bg-scale ~/wallpapers/png/wall.png &
#feh --bg-fill --randomize ~/wallpapers/png &
feh --bg-fill ~/wallpapers/jpg/backiee-279798-landscape.jpg &
#clipmenud &
ssh-add &
dunst &
autotiling &
#killall volumeicon; volumeicon &

#[ ! -s ~/.config/mpd/pid ] && mpd &
#/usr/libexec/polkit-gnome-authentication-agent-1 &
/usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1 &

# NetworkManager is the most popular way to manage wireless networks on Linux,
# and nm-applet is a desktop environment-independent system tray GUI for it.
nm-applet &

# auto lock screen
# xss-lock grabs a logind suspend inhibit lock and will use i3lock-color to lock the
# screen before suspend.
#xss-lock --transfer-sleep-lock -- i3lock --nofork

killall sxhkd; sxhkd -c ~/.config/i3/sxhkd/sxhkdrc &

