#!/usr/bin/env bash
# Wallpaper selector using fzf with per-monitor key bindings.
# Detects connected monitors and creates alt-1..N bindings to apply wallpaper.
# Uses fzf-image-preview.sh for preview.
#
# Usage: wallpaper-selector.sh [wallpaper_directory]
# Dependencies: fzf, fd, jq (macOS, wayland)

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
            # Native panel pixels + orientation. Index matches `desktop N` in osascript.
            # Use _spdisplays_pixels (true panel resolution), not the configured "looks like" value.
            system_profiler SPDisplaysDataType -json 2>/dev/null \
                | jq -r '
                    .SPDisplaysDataType[].spdisplays_ndrvs[]?
                    | select(.spdisplays_online == "spdisplays_yes")
                    | "\(._name)\t\(._spdisplays_pixels)"
                ' \
                | awk -F'\t' '
                    {
                        mon = $1
                        gsub(/[ \/]+/, "-", mon)
                        split($2, p, " ")
                        w = p[1] + 0; h = p[3] + 0
                        ori = (h > w) ? "portrait" : "landscape"
                        i++
                        print "D" i "-" mon "-" ori
                    }
                '
            ;;
        linux)
            if [[ -n "${WAYLAND_DISPLAY:-}" ]]; then
                if command -v swaymsg &>/dev/null; then
                    swaymsg -t get_outputs 2>/dev/null | jq -r '.[] | select(.active) | .name'
                elif command -v hyprctl &>/dev/null; then
                    hyprctl monitors -j 2>/dev/null | jq -r '.[] | select(.disabled | not) | .name'
                elif command -v wlr-randr &>/dev/null; then
                    wlr-randr 2>/dev/null | awk '/^[A-Z]/{name=$1} /Enabled: yes/{print name}'
                fi
            elif [[ -n "${DISPLAY:-}" ]]; then
                xrandr --query | grep " connected" | grep -v "disconnected" | awk '{print $1}'
            fi
            ;;
    esac
}

OS=$(detect_os)
mapfile -t MONITORS < <(detect_monitors)

if [[ ${#MONITORS[@]} -eq 0 ]]; then
    echo "Error: No monitors detected"
    exit 1
fi

# Write monitors to temp file for rofi menu
MONITORS_FILE=$(mktemp /tmp/wallpaper-monitors.XXXXXX)
printf '%s\n' "${MONITORS[@]}" > "$MONITORS_FILE"
trap "rm -f '$MONITORS_FILE'" EXIT

# Header & bindings — style matching fzf.scripts.sh
RESET=$'\033[0m'
GREEN=$'\033[32m'
BOLD=$'\033[1m'
MAGENTA=$'\033[35m'

sep="${MAGENTA}│${RESET}"
iwall=$'\033[38;5;214m󰸉\033[0m'
iapply=$'\033[38;5;32m\033[0m'

header_parts="[${GREEN}${BOLD}enter${RESET}]: Apply ${iapply}"
bind_args=()

for i in "${!MONITORS[@]}"; do
    key_num=$((i + 1))
    monitor="${MONITORS[$i]}"

    header_parts+=" ${sep} [${GREEN}${BOLD}alt-${key_num}${RESET}/${GREEN}${BOLD}f${key_num}${RESET}]: ${monitor} ${iwall}"

    bind_args+=(--bind "alt-${key_num}:execute-silent(${SET_WALLPAPER_CMD} ${monitor} {})")
    bind_args+=(--bind "f${key_num}:execute-silent(${SET_WALLPAPER_CMD} ${monitor} {})")
done

if [[ "$OS" == "macos" ]]; then
    header_parts+=" ${sep} [${GREEN}${BOLD}ctrl-r${RESET}]: Reload ${iapply}"
    bind_args+=(--bind "ctrl-r:execute-silent(${SET_WALLPAPER_CMD} --reload)")
fi

# Enter binding: single monitor → apply directly, multiple → rofi picker
if [[ ${#MONITORS[@]} -eq 1 ]]; then
    bind_args+=(--bind "enter:execute-silent(${SET_WALLPAPER_CMD} ${MONITORS[0]} {})")
else
    bind_args+=(--bind "enter:execute-silent(MONITOR=\$(~/dotfiles/rofi/.config/rofi/menu/menu_launcher.sh < ${MONITORS_FILE}) && [[ -n \$MONITOR ]] && ${SET_WALLPAPER_CMD} \$MONITOR {})")
fi

header=" ${header_parts} "

cd "$WALLPAPER_DIR"

# --preview-border=none
fd --type f -e jpg -e jpeg -e png -e gif -e bmp -e webp -e tiff . | \
    fzf --exact \
    --ansi --height 100% --layout reverse \
    --border --style minimal \
    --info=inline-right \
    --highlight-line --cycle \
    --no-separator \
    --bind 'ctrl-h:toggle-preview' \
    --border-label ' 󰸉 Wallpaper Selector ' \
    --prompt '󰸉 Wallpaper ❯ ' \
    --header "$header" \
    --header-first \
    --preview "${PREVIEW_CMD} {}" \
    --preview-window 'right,60%' \
    "${bind_args[@]}"