#!/usr/bin/env bash

info_path=~/dotfiles/arch/packages-sync

# pacman -Qqe > "$info_path"/pkglist.txt
# pacman -Qqe | grep -v -- '-git$' > "$info_path"/pkglist.txt
pacman -Qqe | grep -Ev -- '(-git|-bin)$' > "$info_path"/pkglist.txt

# pacman -Qqm > "$info_path"/aurlist.txt
pacman -Qqm | grep -v -- '-debug$' > "$info_path"/aurlist.txt