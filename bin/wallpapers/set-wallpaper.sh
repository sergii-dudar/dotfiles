#!/usr/bin/env bash
# Applies a wallpaper to a specific monitor and saves the state.
# Usage: set-wallpaper.sh <monitor_name> <image_path>
#        set-wallpaper.sh --reload
#
# State files are saved to ~/dotfiles/bin/wallpapers/selected/<monitor_name>.txt
# Supports: macOS (osascript), Linux X11 (feh), Linux Wayland (swaybg)
#
# --reload: re-applies every saved state file (used by the macOS reload key).

set -euo pipefail

STATE_DIR="${HOME}/dotfiles/bin/wallpapers/selected"

# macOS-only: maps the fzf button index (D1, D2, ... shown by wallpaper-selector)
# to AppleScript's `desktop N`. Override here if the order system_profiler reports
# does not match how you physically arranged your displays.
# Example for two reversed displays — alt-1 should drive desktop 2, alt-2 drives desktop 1:
#   declare -A MONITOR_MAP=( [1]=2 [2]=1 )
# Any index not listed falls back to identity.
declare -A MONITOR_MAP=( [1]=2 [2]=1 )

if [[ "${1:-}" == "--reload" ]]; then
    shopt -s nullglob
    for state_file in "$STATE_DIR"/*.txt; do
        monitor=$(basename "$state_file" .txt)
        image=$(cat "$state_file")
        if [[ -f "$image" ]]; then
            "${BASH_SOURCE[0]}" "$monitor" "$image"
        fi
    done
    exit 0
fi

if [[ $# -lt 2 ]]; then
    echo "Usage: set-wallpaper.sh <monitor_name> <image_path>"
    echo "       set-wallpaper.sh --reload"
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

apply_macos() {
    # Monitor names are in `D<index>-...` form; <index> is the button index from
    # the fzf selector. MONITOR_MAP (above) translates that to AppleScript desktop N.
    local button_index="${MONITOR_NAME#D}"
    button_index="${button_index%%-*}"

    if ! [[ "$button_index" =~ ^[0-9]+$ ]]; then
        echo "Error: Cannot derive button index from monitor name: $MONITOR_NAME"
        exit 1
    fi

    local desktop_index="${MONITOR_MAP[$button_index]:-$button_index}"

    osascript -e "tell application \"System Events\" to tell desktop ${desktop_index} to set picture to \"${IMAGE_PATH}\""
}

apply_wayland() {
    local pid_file="/tmp/wallpaper-${MONITOR_NAME}.pid"

    notify-send "$MONITOR_NAME" -t 700

    # Kill any swaybg process already running for this monitor (regardless of origin)
    while IFS= read -r pid; do
        kill "$pid" 2>/dev/null || true
    done < <(pgrep -f "swaybg.*-o[[:space:]]+${MONITOR_NAME}" 2>/dev/null || true)

    setsid swaybg -o "$MONITOR_NAME" -i "$IMAGE_PATH" -m fill &
    echo $! > "$pid_file"
    disown
}

OS=$(detect_os)
case "$OS" in
    macos)
        apply_macos
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