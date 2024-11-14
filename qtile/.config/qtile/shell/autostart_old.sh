#!/bin/bash
#


xrandr --output DP-1 --off --output DP-2 --mode 1920x1080 --pos 0x654 --rotate normal --output DP-3 --primary --mode 1920x1080 --pos 1920x0 --rotate normal --output HDMI-1 --mode 1920x1080 --pos 1920x1080 --rotate normal

~/.fehbg &
while pgrep -u $UID -x picom >/dev/null; do sleep 1; done
picom --config /home/matt/.config/picom/picom.conf --vsync &
dunst &
flatpak run com.borgbase.Vorta &
flatpak run com.nextcloud.desktopclient.nextcloud &
/usr/libexec/polkit-gnome-autentication-agent-1 &
~/.fehbg &
clipmenud &
ssh-add &

sxhkd -c $HOME/myrepo/qtile/sxhkd/sxhkdrc &