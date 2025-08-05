#!/bin/sh

SPACE_ICONS=("一" "二" "三" "四" "五" "六" "七" "八" "九" "十" "十一" "十二" "十三" "十四" "十五")

# Destroy space on right click, focus space on left click.
# New space by left clicking separator (>)

sid=0
spaces=()
for i in "${!SPACE_ICONS[@]}"
do
  sid=$(($i+1))
  sketchybar --add space      space.$sid left                               \
             --set space.$sid associated_space=$sid                         \
                              icon=${SPACE_ICONS[i]}                        \
                              icon.padding_left=7                           \
                              icon.padding_right=7                          \
                              icon.background.height=26                     \
                              icon.background.corner_radius=16              \
                              padding_left=2                                \
                              padding_right=4                               \
                              label.padding_right=20                        \
                              label.font="sketchybar-app-font:Regular:16.0" \
                              label.background.height=26                    \
                              label.background.drawing=on                   \
                              label.background.corner_radius=8              \
                              label.drawing=off                             \
                              script="$PLUGIN_DIR/space.sh"                 \
            --subscribe       space.$sid mouse.clicked
done

sketchybar --add bracket spaces '/space\..*/'                   \
           --set spaces  background.color=$BLACK                \
                         background.border_color=$BLACK         \
                         background.border_width=2              \
                         background.drawing=on                  \