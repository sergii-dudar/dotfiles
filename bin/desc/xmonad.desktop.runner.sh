export PATH="$HOME/.local/bin:$PATH"

"$HOME"/dotfiles/bin/wmscripts/autostart_once.sh xmonad
"$HOME"/dotfiles/bin/wmscripts/autostart_always.sh xmonad

exec xmonad

# mkdir -p "$HOME"/.logs
# exec xmonad > "$HOME"/.logs/.xmonad.log 2>&1