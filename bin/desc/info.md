`This desktop runners need only in cause using closed laptop
to be able to propectly disable laptop screen when closed LID
and properly start window manager after it, to properly detect active screens on first start`

`Need remove original *.desctop runner in /usr/share/xsessions with next commands`

```bash
sudo rm /usr/share/xsessions/qtile.desktop ; \
    sudo cp ~/dotfiles/bin/desc/qtile.desktop /usr/share/xsessions/qtile.desktop

sudo rm /usr/share/xsessions/i3.desktop ; \
    sudo cp ~/dotfiles/bin/desc/i3.desktop /usr/share/xsessions/i3.desktop

sudo rm /usr/share/xsessions/awesome.desktop ; \
    sudo cp ~/dotfiles/bin/desc/awesome.desktop /usr/share/xsessions/awesome.desktop

sudo rm /usr/share/xsessions/xmonad.desktop ; \
    sudo cp ~/dotfiles/bin/desc/xmonad.desktop /usr/share/xsessions/xmonad.desktop

sudo rm /usr/share/xsessions/dwm.desktop ; \
    sudo cp ~/dotfiles/bin/desc/dwm.desktop /usr/share/xsessions/dwm.desktop

sudo rm /usr/share/xsessions/bspwm.desktop ; \
    sudo cp ~/dotfiles/bin/desc/bspwm.desktop /usr/share/xsessions/bspwm.desktop

sudo rm /usr/share/xsessions/wm-picker-month.desktop ; \
    sudo cp ~/dotfiles/bin/desc/wm-picker-month.desktop /usr/share/xsessions/wm-picker-month.desktop

sudo rm /usr/share/xsessions/wm-picker-random.desktop ; \
    sudo cp ~/dotfiles/bin/desc/wm-picker-random.desktop /usr/share/xsessions/wm-picker-random.desktop
```