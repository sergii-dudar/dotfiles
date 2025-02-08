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

# killall picom; picom -b --corner-radius 0 --config ~/.config/picom/picom.conf --vsync &

killall picom; picom --backend glx -b --corner-radius 0 --config ~/.config/picom/picom.conf --vsync &

# i3wm specific:
# scratchpad terminal windows
# kitty --name file_namager -e yazi
# kitty --hold --name file_namager -e yazi &

# Start the daemon which listens to focus changes and sets _back mark
i3-back &
