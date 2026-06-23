export PATH="$HOME/.local/bin:$PATH" # manually compiled xmonad by stack placing to ~/.local/bin

"$HOME"/dotfiles/scripts/wmscripts/autostart_once.sh xmonad
"$HOME"/dotfiles/scripts/wmscripts/autostart_always.sh xmonad

exec xmonad

# mkdir -p "$HOME"/.logs
# exec xmonad > "$HOME"/.logs/.xmonad.log 2>&1
