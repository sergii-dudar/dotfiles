#!/bin/bash
#
# idle-inhibitor.sh
# A script to toggle, enable, or disable the system's idle inhibitor on X11.
# This mimics the functionality of a tool like Waybar's idle-inhibitor module.
#
# It works by running `sleep infinity` under `systemd-inhibit`.
# The PID of the sleep process is stored in a file to manage its state.
#
# Usage:
#   ./idle-inhibitor.sh on      - Activates the inhibitor.
#   ./idle-inhibitor.sh off     - Deactivates the inhibitor.
#   ./idle-inhibitor.sh toggle  - Toggles the current state.
#   ./idle-inhibitor.sh status  - Shows the current status.
#

# Use a file in the user's cache directory to store the process ID (PID).
# This is a more robust location than /tmp, which can be cleared on reboot.
PID_FILE="$HOME/.cache/idle-inhibitor.pid"

# Ensure the cache directory exists
mkdir -p "$HOME/.cache"

# --- Functions for each action ---

# Function to activate the inhibitor
activate_inhibitor() {
    if [ -f "$PID_FILE" ]; then
        # Check if the process is actually running, just in case the PID file is stale.
        # kill -0 PID just checks for existence without sending a signal.
        if kill -0 "$(cat "$PID_FILE")" 2>/dev/null; then
            echo "Idle inhibitor is already active."
            exit 0
        fi
    fi

    echo "Activating idle inhibitor..."
    # Start `sleep infinity` with systemd-inhibit.
    # --what=idle:sleep prevents both screen idling and system suspension.
    # The final '&' runs the command in the background.
    systemd-inhibit --what=idle:sleep sleep infinity &

    # The special variable `$!` holds the PID of the last backgrounded process.
    # We store this PID in our state file.
    echo $! > "$PID_FILE"
    echo "Idle inhibitor activated. (PID: $(cat "$PID_FILE"))"
}

# Function to deactivate the inhibitor
deactivate_inhibitor() {
    if [ ! -f "$PID_FILE" ]; then
        echo "Idle inhibitor is not active."
        exit 0
    fi

    # Read the PID from the file
    INHIBITOR_PID=$(cat "$PID_FILE")

    # Check if a process with that PID exists before trying to kill it.
    if kill -0 "$INHIBITOR_PID" 2>/dev/null; then
        echo "Deactivating idle inhibitor (PID: $INHIBITOR_PID)..."
        kill "$INHIBITOR_PID"
        rm "$PID_FILE"
        echo "Idle inhibitor deactivated."
    else
        echo "Inhibitor process not found. Cleaning up stale PID file."
        rm "$PID_FILE"
        exit 1
    fi
}

# Function to show the current status
show_status() {
    if [ -f "$PID_FILE" ] && kill -0 "$(cat "$PID_FILE")" 2>/dev/null; then
        echo " Idle inhibitor is ON (PID: $(cat "$PID_FILE"))"
    else
        echo " Idle inhibitor is OFF"
    fi
}

# --- Main script logic ---

# Use a case statement to handle the command-line argument ($1).
case "$1" in
    on)
        activate_inhibitor
        ;;
    off)
        deactivate_inhibitor
        ;;
    toggle)
        # If the PID file exists and the process is running, turn it off. Otherwise, turn it on.
        if [ -f "$PID_FILE" ] && kill -0 "$(cat "$PID_FILE")" 2>/dev/null; then
            deactivate_inhibitor
        else
            activate_inhibitor
        fi
        ;;
    status)
        show_status
        ;;
    *)
        # If no valid argument is given, print the usage instructions.
        echo "Usage: $0 {on|off|toggle|status}"
        exit 1
        ;;
esac

exit 0