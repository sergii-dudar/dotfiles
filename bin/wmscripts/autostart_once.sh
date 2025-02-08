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

wm_name="${1:-}"

case "$wm_name" in
    "i3")
        killall sxhkd; sxhkd -c ~/.config/sxhkd/i3/sxhkdrc &
        ~/.config/polybar/i3/launch.sh &

        # Start the daemon which listens to focus changes and sets _back mark
        i3-back &

        # i3wm specific:
        # scratchpad terminal windows
        # kitty --name file_namager -e yazi
        # kitty --hold --name file_namager -e yazi &

        ;;
    "dwm")
        dwmblocks &
        killall sxhkd; sxhkd -c ~/.config/sxhkd/dwm/sxhkdrc &
        ;;
    "bspwm")
        killall sxhkd; sxhkd -c ~/.config/sxhkd/bspwm/sxhkdrc &
        ~/.config/polybar/bspwm/launch.sh &
        ;;
    *)
        killall sxhkd; sxhkd -c ~/.config/sxhkd/sxhkdrc &
        ;;
esac

if [[ "$wm_name" == "i3" ]]; then
    # i3 working not well with rounded gaps, disable it
    killall picom; picom --backend glx -b --corner-radius 0 --config ~/.config/picom/picom.conf --vsync &
else
    killall picom; picom --backend glx -b --config ~/.config/picom/picom.conf --vsync &
fi
