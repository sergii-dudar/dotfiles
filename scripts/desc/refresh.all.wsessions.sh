#!/usr/bin/env bash

sudo rm /usr/share/wayland-sessions/sway.desktop ; \
    sudo cp ~/dotfiles/scripts/desc/sway.desktop /usr/share/wayland-sessions/sway.desktop

sudo rm /usr/share/wayland-sessions/hyprland.desktop ; \
    sudo cp ~/dotfiles/scripts/desc/hyprland.desktop /usr/share/wayland-sessions/hyprland.desktop

sudo rm /usr/share/wayland-sessions/dwl.desktop ; \
    sudo cp ~/dotfiles/scripts/desc/dwl.desktop /usr/share/wayland-sessions/dwl.desktop
