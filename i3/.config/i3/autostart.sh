#!/bin/sh


#xrandr --output DP-1 --off --output DP-2 --mode 1920x1080 --pos 0x654 --rotate normal --output DP-3 --primary --mode 1920x1080 --pos 1920x0 --rotate normal --output HDMI-1 --mode 1920x1080 --pos 1920x1080 --rotate normal
~/dotfiles/scripts/offlaptop-screen-if-closed.sh &


# compositor
killall picom &
#while pgrep -u $UID -x picom >/dev/null; do sleep 1; done
picom --config ~/.config/picom/picom.conf --vsync &
#flatpak run com.borgbase.Vorta &

~/.config/polybar/launch.sh &

#bg
#nitrogen --restore &
~/.fehbg &
#clipmenud &
#ssh-add &
#dunst &
autotiling &

#[ ! -s ~/.config/mpd/pid ] && mpd &
#/usr/libexec/polkit-gnome-authentication-agent-1 &
#/usr/lib/polkit-kde-authentication-agent-1 &

#sxhkd >/dev/null 2>&1 &
sxhkd -c ~/.config/i3/sxhkd/sxhkdrc &