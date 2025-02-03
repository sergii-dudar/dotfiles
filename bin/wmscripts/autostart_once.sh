#!/usr/bin/env bash

# "$HOME"/dotfiles/bin/apply-display-settings.sh &
~/dotfiles/bin/apply-display-settings.sh
#xsettingsd &
ssh-add &
dunst &
/usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1 &

# Automatically lock the screen after 10 minutes of inactivity
xautolock -time 10 -locker "$HOME/dotfiles/bin/screen-lock" -detectsleep &

# NetworkManager is the most popular way to manage wireless networks on Linux,
# and nm-applet is a desktop environment-independent system tray GUI for it.

nm-applet &
#volumeicon &
#glate &
#kitty --hold zsh -c "yazi" &
#wezterm &
#kitty &
ghostty --class=com.ghostty.group01 &

#killall volumeicon; volumeicon &
#google-chrome-stable &
brave &

# google chat
# google-chrome-stable --app-id=crx_mdpkiolbdkhdjpekfbkbmhigcaggjagi &
#telegram-desktop &

# scratchpad terminal windows
# kitty --name file_namager -e yazi
# kitty --hold --name file_namager -e yazi &

#[ ! -s ~/.config/mpd/pid ] && mpd &
#/usr/libexec/polkit-gnome-authentication-agent-1 &

# move it move autostart always, in case want to add change and apply on rm restart
killall picom; picom --backend glx -b --config ~/.config/picom/picom.conf --vsync &

sxhkd_subdir="${1:-}"
killall sxhkd; sxhkd -c ~/.config/sxhkd/${sxhkd_subdir}sxhkdrc &