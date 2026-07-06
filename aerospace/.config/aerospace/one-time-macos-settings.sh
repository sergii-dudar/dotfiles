#!/usr/bin/env bash

# source: https://nikitabobko.github.io/AeroSpace/guide

defaults write com.apple.dock expose-group-apps -bool true && killall Dock
defaults write com.apple.spaces spans-displays -bool true && killall SystemUIServer