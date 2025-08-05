sketchybar --add item pomo_history left \
           --set pomo_history \
             label="" \
             drawing=off \
             label.align=center \
             icon.align=center \
             padding_right=9                    \
             update_freq=1800 \
             script="$PLUGIN_DIR/pomodoro_history.sh" \
             click_script="$PLUGIN_DIR/pomodoro_history.sh"
