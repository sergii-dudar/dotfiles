#!/bin/bash

function run {
  if ! pgrep $1 ;
  then
    $@&
  fi
}

xrandr --output DP-1 --off --output DP-2 --mode 1920x1080 --pos 1920x415 --rotate normal --output DP-3 --primary --mode 1920x1080 --pos 0x884 --rotate normal --output HDMI-1 --mode 1920x1080 --pos 1920x1495 --rotate normal

#start mpd
[ ! -s ~/.config/mpd/pid ] && mpd &


sxhkd -c ~/.config/xmonad/scripts/sxhkdrc &
setxkbmap -option ctrl:nocaps &
clipmenud &
ssh-add &
dunst &
emacs --daemon &

#starting utility applications at boot time
picom --config $HOME/.config/picom/picom.conf --vsync &
/usr/libexec/polkit-gnome-autentication-agent-1 &
#/usr/lib/xfce4/notifyd/xfce4-notifyd &
#deckmaster -deck ~/.config/deck/main.deck &
playerctld daemon &
xhost +si:localuser:$USER &
~/.fehbg &
