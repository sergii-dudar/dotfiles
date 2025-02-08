#!/usr/bin/env bash

info_path=~/dotfiles/arch/packages-sync

sudo pacman -S --needed - < "$info_path"/pkglist.txt
yay -S --needed - < "$info_path"/aurlist.txt