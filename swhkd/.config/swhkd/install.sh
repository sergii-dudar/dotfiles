#!/usr/bin/env bash

cd "$HOME/dotfiles/swhkd/.config/swhkd" || exit 1
git clone https://github.com/waycrate/swhkd ; cd swhkd || exit 1
make setup
make clean
make
sudo make install