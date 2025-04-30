# stack install xmonad

----------------------------------

## ghcup list

- hls 2.9.0.1
- stack 3.1.1
- hls 2.9.0.1
- ghcup install ghc 9.6.6 --set
- stack setup 9.6.6
- cd ~/.config/xmonad/ && stack init && stack clean && stack build

# ghc 9.6.6 to match haskell-language-server-9.6.6 by nvim h tools, as it should match to lsp work properly

## dependencies

```bash

sudo pacman -S xdotool xdo trayer xmobar 

```

----------------------------------
git clone <https://github.com/xmonad/xmonad>
git clone <https://github.com/xmonad/xmonad-contrib>
