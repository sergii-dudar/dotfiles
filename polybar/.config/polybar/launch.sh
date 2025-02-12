#!/usr/bin/env bash

# Terminate already running bar instances
# If all your bars have ipc enabled, you can use
# killall -q polybar


# wm_name="${1:-}"
wm_name="${1:-i3}"

# Otherwise you can use the nuclear option:
killall -q polybar

# Wait until the processes have been shut down
while pgrep -u $UID -x polybar >/dev/null; do sleep 1; done

# Launch bar1 and bar2
echo "---" | tee -a /tmp/polybar1.log /tmp/polybar2.log
polybar -c ~/.config/polybar/config.ini mainbar-"$wm_name" 2>&1 | tee -a /tmp/polybar1.log & disown

echo "$wm_name: bars launched..."