#!/bin/sh

source "$HOME/.config/sketchybar/colors.sh"

COUNT=$(brew outdated | wc -l | tr -d ' ')

COLOR=$RED

case "$COUNT" in
  [3-5][0-9]) COLOR=$MAGENTA
  ;;
  [1-2][0-9]) COLOR=$ORANGE
  ;;
  [1-9]) COLOR=$GREEN
  ;;
  0) COLOR=$YELLOW
     COUNT=􀆅
  ;;
esac

sketchybar --set $NAME label=$COUNT icon.color=$COLOR
