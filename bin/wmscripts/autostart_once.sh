#!/usr/bin/env bash

~/dotfiles/bin/apply-display-settings.sh
ssh-add &
dunst &
/usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1 &
# telegram-desktop &

# Automatically lock the screen after 10 minutes of inactivity
xautolock -time 10 -locker "$HOME/dotfiles/bin/screen-lock" -detectsleep &

ghostty --class=com.ghostty.group01 &
#glate &
nm-applet &

#google-chrome-stable &
brave &

killall picom; picom --backend glx -b --config ~/.config/picom/picom.conf --vsync &

sxhkd_subdir="${1:-}"
killall sxhkd; sxhkd -c ~/.config/sxhkd/${sxhkd_subdir}sxhkdrc &