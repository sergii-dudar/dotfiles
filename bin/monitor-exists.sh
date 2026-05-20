#!/usr/bin/env bash
# Checks whether a monitor is currently connected and active.
# Usage: monitor-exists.sh <monitor_name>
# Exit code: 0 if found, 1 if not found.

set -euo pipefail

if [[ $# -lt 1 ]]; then
    echo "Usage: monitor-exists.sh <monitor_name>"
    exit 1
fi

MONITOR_NAME="$1"

list_monitors() {
    case "$(uname -s)" in
        Linux)
            if [[ -n "${WAYLAND_DISPLAY:-}" ]]; then
                if command -v hyprctl &>/dev/null && hyprctl monitors -j &>/dev/null 2>&1; then
                    hyprctl monitors -j 2>/dev/null | jq -r '.[] | select(.disabled | not) | .name'
                elif command -v swaymsg &>/dev/null && swaymsg -t get_outputs &>/dev/null 2>&1; then
                    swaymsg -t get_outputs 2>/dev/null | jq -r '.[] | select(.active) | .name'
                elif command -v wlr-randr &>/dev/null; then
                    wlr-randr 2>/dev/null | awk '/^[A-Z]/{name=$1} /Enabled: yes/{print name}'
                fi
            elif [[ -n "${DISPLAY:-}" ]]; then
                xrandr --query | grep " connected" | grep -v "disconnected" | awk '{print $1}'
            fi
            ;;
        *)
            echo "Error: Unsupported OS: $(uname -s)" >&2
            exit 1
            ;;
    esac
}

if list_monitors | grep -qx "$MONITOR_NAME"; then
    exit 0
else
    exit 1
fi
