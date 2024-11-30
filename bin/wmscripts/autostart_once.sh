#!/usr/bin/env bash

~/dotfiles/bin/apply-display-settings.sh &
#xsettingsd &
ssh-add &
dunst &
/usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1 &

#glate &
#kitty --hold zsh -c "yazi" &
wezterm &
kitty &

#killall volumeicon; volumeicon &
google-chrome-stable &

# google chat
# google-chrome-stable --app-id=crx_mdpkiolbdkhdjpekfbkbmhigcaggjagi &
#telegram-desktop &

# scratchpad terminal windows
# kitty --name file_namager -e yazi
# kitty --hold --name file_namager -e yazi &

#[ ! -s ~/.config/mpd/pid ] && mpd &
#/usr/libexec/polkit-gnome-authentication-agent-1 &