glate &
kitty --hold zsh -c "yazi" &
wezterm &
#clipmenud &
ssh-add &
dunst &
#killall volumeicon; volumeicon &
google-chrome-stable &

# google chat
google-chrome-stable --app-id=crx_mdpkiolbdkhdjpekfbkbmhigcaggjagi &
telegram-desktop &

# scratchpad terminal windows
# kitty --name file_namager -e yazi
kitty --hold --name file_namager -e yazi &

# Start the daemon which listens to focus changes and sets _back mark
i3-back &

#[ ! -s ~/.config/mpd/pid ] && mpd &
#/usr/libexec/polkit-gnome-authentication-agent-1 &
/usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1 &