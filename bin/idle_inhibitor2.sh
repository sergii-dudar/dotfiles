#!/bin/bash

# File to store inhibitor state
STATE_FILE="$HOME/.idle_inhibitor_state"
# File to store xautolock PID
PID_FILE="$HOME/.xautolock_pid"

# Function to check if xautolock is installed
check_xautolock() {
    if ! command -v xautolock &> /dev/null; then
        echo "Error: xautolock is not installed. Please install it first."
        exit 1
    fi
}

# Function to start xautolock with default settings
start_xautolock() {
    # Kill any existing xautolock instance
    if [ -f "$PID_FILE" ]; then
        kill "$(cat "$PID_FILE")" 2>/dev/null
        rm -f "$PID_FILE"
    fi
    # Start xautolock with default settings (e.g., lock after 10 minutes)
    xautolock -time 10 -locker "i3lock -c 000000" &
    echo $! > "$PID_FILE"
    echo "Idle inhibitor OFF: xautolock started."
    echo "off" > "$STATE_FILE"
}

# Function to disable xautolock (enable inhibitor)
disable_xautolock() {
    if [ -f "$PID_FILE" ]; then
        kill "$(cat "$PID_FILE")" 2>/dev/null
        rm -f "$PID_FILE"
        echo "Idle inhibitor ON: xautolock stopped."
        echo "on" > "$STATE_FILE"
    else
        echo "Idle inhibitor ON: xautolock was not running."
        echo "on" > "$STATE_FILE"
    fi
}

# Function to toggle inhibitor
toggle_inhibitor() {
    check_xautolock
    if [ -f "$STATE_FILE" ] && [ "$(cat "$STATE_FILE")" = "on" ]; then
        start_xautolock
    else
        disable_xautolock
    fi
}

# Function to get status
get_status() {
    if [ -f "$STATE_FILE" ]; then
        state=$(cat "$STATE_FILE")
        if [ "$state" = "on" ]; then
            echo "Idle inhibitor: ON"
        else
            echo "Idle inhibitor: OFF"
        fi
    else
        echo "Idle inhibitor: OFF (no state file)"
    fi
}

# Main script logic
case "$1" in
    toggle)
        toggle_inhibitor
        ;;
    status)
        get_status
        ;;
    *)
        echo "Usage: $0 {toggle|status}"
        exit 1
        ;;
esac