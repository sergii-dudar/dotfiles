#!/bin/sh
source "$HOME/.config/sketchybar/colors.sh"

update() {
  WIDTH="dynamic"
  if [ "$SELECTED" = "true" ]; then
        sketchybar --animate tanh 20 --set $NAME \
            icon.color=$BLACK \
            icon.background.color=$YELLOW \
            background.color=$YELLOW \
            label.color=$BLACK \
            label.background.color=$YELLOW \
            background.border_color=$BACKGROUND_2        \
            background.border_width=1

  else
      sketchybar --animate tanh 20 --set $NAME \
          icon.color=$YELLOW \
          icon.background.color=$BACKGROUND_1 \
          background.color=$BACKGROUND_1 \
          label.color=$YELLOW \
          label.background.color=$BACKGROUND_1 \
          background.border_color=$BACKGROUND_2        \
          background.border_width=1
  fi
}

mouse_clicked() {
  if [ "$BUTTON" = "right" ]; then
    yabai -m space --destroy $SID
    sketchybar --trigger space_change
  else
    yabai -m space --focus $SID 2>/dev/null
  fi
}

case "$SENDER" in
  "mouse.clicked") mouse_clicked
  ;;
  *) update
  ;;
esac