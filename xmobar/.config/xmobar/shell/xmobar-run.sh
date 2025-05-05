# xmobar ~/dotfiles/xmobar/.config/xmobar/xmobarrc.hs
export PATH="$PATH:$HOME/.local/bin"
export PATH="$PATH:$HOME/.ghcup/bin"
cd ~/dotfiles/xmobar/.config/xmobar && stack build && stack exec xmobar-config -- -x "$1"