#!/bin/bash

# https://snapcraft.io/docs/installing-snap-on-arch-linux#confinement

cd ~/tools && \
git clone https://aur.archlinux.org/snapd.git && \
cd snapd && \
makepkg -si && \
sudo systemctl enable --now snapd.socket && \
sudo pacman -S apparmor && \
sudo systemctl enable --now snapd.apparmor.service && \
sudo ln -s /var/lib/snapd/snap /snap && \
snap install glate