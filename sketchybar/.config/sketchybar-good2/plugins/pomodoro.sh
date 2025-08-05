#!/bin/bash
source "$HOME/.config/sketchybar/colors.sh"

WORK_MIN=50
BREAK_MIN=10

POMO_DIR="$HOME/.config/sketchybar/pomodoro"
POMO_HISTORY_DIR="$HOME/projects/footprints/pomodoro"
MODE_FILE="$POMO_DIR/pomo_mode"
PID_FILE="$POMO_DIR/pomo_timer.pid"
HISTORY_FILE="$POMO_HISTORY_DIR/.pomodoro_history"

mkdir -p "$POMO_DIR"

BUTTON_WORK="work_button"
BUTTON_BREAK="break_button"

start_timer() {
    local duration="$1"
    local button="$2"
    local label="$3"

    local START_TIME=$(date '+%Y-%m-%d %H:%M:%S')

    (
      TIME_LEFT=$((duration * 60))
      while [ $TIME_LEFT -ge 0 ]; do
          MINUTES=$((TIME_LEFT / 60))
          SECONDS=$((TIME_LEFT % 60))
          TIME_STR=$(printf "%02d:%02d" $MINUTES $SECONDS)
          sketchybar --set "$button" label="$label $TIME_STR" label.color=$YELLOW drawing=on 
          sleep 1
          TIME_LEFT=$((TIME_LEFT - 1))
      done

      local END_TIME=$(date '+%Y-%m-%d %H:%M:%S')  # Save end time now

      # Send macOS notification after timer ends
      if [ "$label" = "🍅" ]; then
          terminal-notifier \
            -title 'Pomodoro'\
			-message 'Work Timer is up! Take a Break 🍅'\
			-appIcon "$HOME/.config/sketchybar/img/pomodoro.png"\
			-sound Funk
          echo "$START_TIME  $END_TIME  [WORK]  $WORK_MIN mins" >> "$HISTORY_FILE"
      elif [ "$label" = "☕️" ]; then
          terminal-notifier \
            -title 'Pomodoro'\
            -message 'Break is over! Get back to work ☕️'\
			-appIcon "$HOME/.config/sketchybar/img/pomodoro.png"\
			-sound Funk
          echo "$START_TIME  $END_TIME  [REST]  $BREAK_MIN mins" >> "$HISTORY_FILE"

      fi
      # Clean up PID file after timer naturally ends
      rm -f "$PID_FILE"

      # Reset UI after timer completion
      sketchybar --set $BUTTON_WORK label="🍅" drawing=on \
                 --set $BUTTON_BREAK label="☕️" drawing=on
      echo "none" > "$MODE_FILE"
    ) &
    echo $! > "$PID_FILE"
}

stop_timer() {
    # Stop and clean up timer process
    if [ -f "$PID_FILE" ]; then
        kill $(cat "$PID_FILE") 2>/dev/null
        rm -f "$PID_FILE"
    fi
    echo "none" > "$MODE_FILE"
    sketchybar --set $BUTTON_WORK label="🍅" drawing=on \
               --set $BUTTON_BREAK label="☕️" drawing=on
}

case "$NAME" in
  "work_button")
    current_mode=$(cat "$MODE_FILE" 2>/dev/null)
    if [ "$current_mode" = "work" ]; then
        stop_timer
    else
        stop_timer
        echo "work" > "$MODE_FILE"
        # 片方オだけオフにすると一度アイコンが真ん中に移動し、start_timerでアイコンの位置が変わりチラつく
        # そのチラつきへの対策で、一度全てオフにすることでチラつきを減らしている。
        sketchybar --set $BUTTON_WORK drawing=off \
                   --set $BUTTON_BREAK drawing=off
        start_timer $WORK_MIN "$BUTTON_WORK" "🍅"
    fi
    ;;
  "break_button")
    current_mode=$(cat "$MODE_FILE" 2>/dev/null)
    if [ "$current_mode" = "break" ]; then
        stop_timer
    else
        stop_timer
        echo "break" > "$MODE_FILE"
        # 片方オだけオフにすると一度アイコンが真ん中に移動し、start_timerでアイコンの位置が変わりチラつく
        # そのチラつきへの対策で、一度全てオフにすることでチラつきを減らしている。
        sketchybar --set $BUTTON_WORK drawing=off \
                   --set $BUTTON_BREAK drawing=off
        start_timer $BREAK_MIN "$BUTTON_BREAK" "☕️"
    fi
    ;;
  *)
    ;;
esac
