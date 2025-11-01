#!/usr/bin/env bash

info_path=~/dotfiles/installed-packages/arch-linux

yay -S --needed - < "$info_path"/pkglist.txt
yay -S --needed - < "$info_path"/aurlist.txt