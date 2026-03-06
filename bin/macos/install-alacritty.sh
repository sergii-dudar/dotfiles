#!/usr/bin/env bash
# details: https://github.com/alacritty/alacritty/blob/master/INSTALL.md

# mkdir -p ~/tools/tests
# git clone https://github.com/alacritty/alacritty.git
cd ~/tools/tests/alacritty
git pull

cargo build --release
make app
cp -r target/release/osx/Alacritty.app /Applications/