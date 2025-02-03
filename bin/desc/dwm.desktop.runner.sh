"$HOME"/dotfiles/bin/wmscripts/autostart_once.sh dwm/
"$HOME"/dotfiles/bin/wmscripts/autostart_always.sh

dwmblocks &

exec dwm
# mkdir -p "$HOME"/.logs
# exec dwm > "$HOME"/.logs/.dwm.log 2>&1