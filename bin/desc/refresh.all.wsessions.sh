#!/usr/bin/env bash

sudo rm /usr/share/wayland-sessions/sway.desktop ; \
    sudo cp ~/dotfiles/bin/desc/sway.desktop /usr/share/wayland-sessions/sway.desktop

sudo rm /usr/share/wayland-sessions/hyprland.desktop ; \
    sudo cp ~/dotfiles/bin/desc/hyprland.desktop /usr/share/wayland-sessions/hyprland.desktop
