#!/usr/bin/env bash

sudo rm /usr/share/xsessions/i3.desktop ; \
    sudo cp ~/dotfiles/scripts/desc/i3.desktop /usr/share/xsessions/i3.desktop

sudo rm /usr/share/xsessions/qtile.desktop ; \
    sudo cp ~/dotfiles/scripts/desc/qtile.desktop /usr/share/xsessions/qtile.desktop

sudo rm /usr/share/xsessions/awesome.desktop ; \
    sudo cp ~/dotfiles/scripts/desc/awesome.desktop /usr/share/xsessions/awesome.desktop

sudo rm /usr/share/xsessions/dwm.desktop ; \
    sudo cp ~/dotfiles/scripts/desc/dwm.desktop /usr/share/xsessions/dwm.desktop

sudo rm /usr/share/xsessions/bspwm.desktop ; \
    sudo cp ~/dotfiles/scripts/desc/bspwm.desktop /usr/share/xsessions/bspwm.desktop

sudo rm /usr/share/xsessions/xmonad.desktop ; \
    sudo cp ~/dotfiles/scripts/desc/xmonad.desktop /usr/share/xsessions/xmonad.desktop

sudo rm /usr/share/xsessions/wm-picker-month.desktop ; \
    sudo cp ~/dotfiles/scripts/desc/wm-picker-month.desktop /usr/share/xsessions/wm-picker-month.desktop

sudo rm /usr/share/xsessions/wm-picker-random.desktop ; \
    sudo cp ~/dotfiles/scripts/desc/wm-picker-random.desktop /usr/share/xsessions/wm-picker-random.desktop
