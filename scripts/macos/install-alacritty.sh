#!/usr/bin/env bash
# details: https://github.com/alacritty/alacritty/blob/master/INSTALL.md

# mkdir -p ~/tools/tests
# git clone https://github.com/alacritty/alacritty.git
cd ~/tools/tests/alacritty
git pull

cargo build --release
make app
cp -r target/release/osx/Alacritty.app /Applications/

# TERM=alacritty needs its terminfo; Alacritty.app does not ship it and macOS ncurses predates it.
# Without it alacritty falls back to xterm-256color and tmux gives it only the generic xterm features
# (no undercurl / strikethrough / hyperlinks) -- see the alacritty* terminal-features line in tmux.conf.
# -o: explicit target, because a shell started from Ghostty has TERMINFO pointing into Ghostty.app and tic would write there.
tic -o ~/.terminfo -xe alacritty,alacritty-direct ~/tools/tests/alacritty/extra/alacritty.info