#!/usr/bin/env bash

LOCK_CMD="i3lock -c 000000"  # or your preferred lock command
PIDFILE="/tmp/.xautolock.pid"

toggle_inhibitor() {
    if [ -f "$PIDFILE" ] && kill -0 "$(cat "$PIDFILE")" 2>/dev/null; then
        echo "Disabling idle inhibitor..."
        kill "$(cat "$PIDFILE")"
        rm -f "$PIDFILE"
    else
        echo "Enabling idle inhibitor..."
        xautolock -time 5 -locker "$LOCK_CMD" &
        echo $! > "$PIDFILE"
    fi
}

status() {
    if [ -f "$PIDFILE" ] && kill -0 "$(cat "$PIDFILE")" 2>/dev/null; then
        echo "Idle inhibitor is ENABLED"
    else
        echo "Idle inhibitor is DISABLED"
    fi
}

case "$1" in
    toggle) toggle_inhibitor ;;
    status) status ;;
    *) echo "Usage: $0 {toggle|status}" ;;
esac