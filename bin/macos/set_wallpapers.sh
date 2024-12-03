#!/usr/bin/env bash

# Paths to the wallpaper images
WALLPAPER1="~/wallpapers/jpg/Wall147.jpg"
WALLPAPER2="~/wallpapers/jpg/1390920427025.jpg"

# Set wallpaper for monitor 1
osascript -e "tell application \"System Events\" to tell desktop 1 to set picture to \"$WALLPAPER1\""

# Set wallpaper for monitor 2
osascript -e "tell application \"System Events\" to tell desktop 2 to set picture to \"$WALLPAPER2\""