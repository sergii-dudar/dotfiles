#!/usr/bin/env bash
# Wallpaper selector using fzf with per-monitor key bindings.
# Detects connected monitors and creates alt-1..N bindings to apply wallpaper.
# Uses fzf-image-preview.sh for preview.
#
# Usage: wallpaper-selector.sh [wallpaper_directory]
# Dependencies: fzf, fd, jq (wayland)

set -euo pipefail

WALLPAPER_DIR="${1:-$HOME/wallpapers}"
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SET_WALLPAPER_CMD="${SCRIPT_DIR}/set-wallpaper.sh"
PREVIEW_CMD="${SCRIPT_DIR}/fzf-image-preview.sh"

if [[ ! -d "$WALLPAPER_DIR" ]]; then
    echo "Error: Wallpaper directory not found: $WALLPAPER_DIR"
    exit 1
fi

detect_os() {
    case "$(uname -s)" in
        Darwin) echo "macos" ;;
        Linux)  echo "linux" ;;
        *)      echo "unknown" ;;
    esac
}

detect_monitors() {
    local os
    os=$(detect_os)

    case "$os" in
        macos)
            # TODO: improve macOS monitor detection
            system_profiler SPDisplaysDataType 2>/dev/null \
                | awk '/^ +[^ ]/{name=$0} /Resolution:/{gsub(/^ +| +$/,"",name); gsub(/:$/,"",name); print name}'
            ;;
        linux)
            if [[ -n "${WAYLAND_DISPLAY:-}" ]]; then
                if command -v swaymsg &>/dev/null; then
                    swaymsg -t get_outputs 2>/dev/null | jq -r '.[].name'
                elif command -v hyprctl &>/dev/null; then
                    hyprctl monitors -j 2>/dev/null | jq -r '.[].name'
                elif command -v wlr-randr &>/dev/null; then
                    wlr-randr 2>/dev/null | awk '/^[A-Z]/{print $1}'
                fi
            elif [[ -n "${DISPLAY:-}" ]]; then
                xrandr --query | grep " connected" | awk '{print $1}'
            fi
            ;;
    esac
}

mapfile -t MONITORS < <(detect_monitors)

if [[ ${#MONITORS[@]} -eq 0 ]]; then
    echo "Error: No monitors detected"
    exit 1
fi

# Header & bindings — style matching fzf.scripts.sh
RESET=$'\033[0m'
GREEN=$'\033[32m'
BOLD=$'\033[1m'
MAGENTA=$'\033[35m'

sep="${MAGENTA}│${RESET}"
iwall=$'\033[38;5;214m󰸉\033[0m'

header_parts=""
bind_args=()

for i in "${!MONITORS[@]}"; do
    key_num=$((i + 1))
    monitor="${MONITORS[$i]}"

    if [[ -n "$header_parts" ]]; then
        header_parts+=" ${sep} "
    fi
    header_parts+="[${GREEN}${BOLD}alt-${key_num}${RESET}]: ${monitor} ${iwall}"

    bind_args+=(--bind "alt-${key_num}:execute-silent(${SET_WALLPAPER_CMD} ${monitor} {})")
done

header=" ${header_parts} "

cd "$WALLPAPER_DIR"

fd --type f -e jpg -e jpeg -e png -e gif -e bmp -e webp -e tiff . | \
    fzf --exact \
        --border-label ' 󰸉 Wallpaper Selector ' \
        --prompt '󰸉 Wallpaper ❯ ' \
        --header "$header" \
        --header-first \
        --preview "${PREVIEW_CMD} {}" \
        --preview-window 'right,60%' \
        "${bind_args[@]}"
