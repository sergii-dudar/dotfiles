#!/usr/bin/env bash
# Applies a wallpaper to a specific monitor and saves the state.
# Usage: set-wallpaper.sh <monitor_name> <image_path>
#
# State files are saved to ~/.config/wallpapers/<monitor_name>.txt
# Supports: macOS (TODO), Linux X11 (feh), Linux Wayland (swaybg)

set -euo pipefail

STATE_DIR="${HOME}/dotfiles/bin/wallpapers/selected"

if [[ $# -lt 2 ]]; then
    echo "Usage: set-wallpaper.sh <monitor_name> <image_path>"
    exit 1
fi

MONITOR_NAME="$1"
IMAGE_PATH="$(realpath "$2")"

if [[ ! -f "$IMAGE_PATH" ]]; then
    echo "Error: File not found: $IMAGE_PATH"
    exit 1
fi

mkdir -p "$STATE_DIR"
echo "$IMAGE_PATH" > "${STATE_DIR}/${MONITOR_NAME}.txt"

detect_os() {
    case "$(uname -s)" in
        Darwin) echo "macos" ;;
        Linux)  echo "linux" ;;
        *)      echo "unknown" ;;
    esac
}

detect_display_server() {
    if [[ -n "${WAYLAND_DISPLAY:-}" ]]; then
        echo "wayland"
    elif [[ -n "${DISPLAY:-}" ]]; then
        echo "x11"
    else
        echo "unknown"
    fi
}

apply_x11() {
    # feh sets wallpapers in xrandr monitor order — read all state files
    # to reconstruct the full command
    local -a images=()
    while IFS= read -r monitor; do
        local state_file="${STATE_DIR}/${monitor}.txt"
        if [[ -f "$state_file" ]]; then
            images+=("$(cat "$state_file")")
        else
            images+=("$IMAGE_PATH")
        fi
    done < <(xrandr --query | grep " connected" | grep -v "disconnected" | awk '{print $1}')

    if [[ ${#images[@]} -gt 0 ]]; then
        feh --bg-fill "${images[@]}"
    fi
}

apply_wayland() {
    local pid_file="/tmp/wallpaper-${MONITOR_NAME}.pid"

    if [[ -f "$pid_file" ]]; then
        local old_pid
        old_pid=$(cat "$pid_file")
        kill "$old_pid" 2>/dev/null || true
    fi

    swaybg -i "$IMAGE_PATH" -m fill -o "$MONITOR_NAME" &
    echo $! > "$pid_file"
    disown
}

OS=$(detect_os)
case "$OS" in
    macos)
        # TODO: macOS wallpaper support will be added later
        echo "macOS wallpaper support not yet implemented"
        ;;
    linux)
        DISPLAY_SERVER=$(detect_display_server)
        case "$DISPLAY_SERVER" in
            x11)     apply_x11 ;;
            wayland) apply_wayland ;;
            *)
                echo "Error: Could not detect display server"
                exit 1
                ;;
        esac
        ;;
    *)
        echo "Error: Unsupported OS: $(uname -s)"
        exit 1
        ;;
esac

echo "Wallpaper set for ${MONITOR_NAME}: $(basename "$IMAGE_PATH")"
