"$HOME/dotfiles/bin/wmscripts/autostart_once.sh"
"$HOME/dotfiles/bin/wmscripts/autostart_always.sh"

# temp:
killall sxhkd; sxhkd -c ~/.config/sxhkd/dwm/sxhkdrc &

# exec dwm
mkdir -p "$HOME"/.logs
exec dwm > "$HOME"/.logs/.dwm.log 2>&1