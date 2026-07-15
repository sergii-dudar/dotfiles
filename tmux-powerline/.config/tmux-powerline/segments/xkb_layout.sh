# shellcheck shell=bash
# Print the currently used keyboard layout
# This depends on a specifically developed program which prints the group id of
# the currently used layout.
# I developed the simple program myself with some guidance as I was unable to
# find anything already developed.
# Some people might suggest:
# $ setxkbmod -query -v | awk -F "+" '{print $2}'
# this will only work if you have set up XKB with a single layout which is true
# for some.

# This script will print the correct layout even if layout is set per window.
# Exit if platform is not linux as this script is dependant on X11

TMUX_POWERLINE_SEG_XKB_LAYOUT_ICON="${TMUX_POWERLINE_SEG_XKB_LAYOUT_ICON:-⌨ }"

generate_segmentrc() {
    read -r -d '' rccontents <<EORC
# Keyboard icon
export TMUX_POWERLINE_SEG_XKB_LAYOUT_ICON="${TMUX_POWERLINE_SEG_XKB_LAYOUT_ICON}"
EORC
    echo "$rccontents"
}

# OS_TYPE=$(uname)
# function isMacOs() {
#     if [[ "$OS_TYPE" == "Darwin" ]]; then
#         #echo "current is MacOs..."
#         return 0  # true
#     else
#         #echo "current is Linux..."
#         return 1  # false
#     fi
# }

get_macos_input_source_label() {
    local source_id

    source_id=$(defaults read "$HOME/Library/Preferences/com.apple.HIToolbox.plist" AppleCurrentKeyboardLayoutInputSourceID 2>/dev/null) || return 1

    case "$source_id" in
        com.apple.keylayout.ABC)
            echo "US"
            ;;
        com.apple.keylayout.Ukrainian-PC)
            echo "UA"
            ;;
        *)
            echo "${source_id##*.}"
            ;;
    esac
}

run_segment() {
    case "$(uname)" in
        "Darwin")
            cur_layout=$(get_macos_input_source_label) || return 1
            echo "$TMUX_POWERLINE_SEG_XKB_LAYOUT_ICON $(echo "$cur_layout" | tr '[:lower:]' '[:upper:]')"
            ;;
        "Linux")
            case "$XDG_SESSION_TYPE" in
                "x11")
                    cd "$TMUX_POWERLINE_DIR_SEGMENTS" || return
                    if [ ! -x "xkb_layout" ]; then
                        make clean xkb_layout &>/dev/null
                    fi

                    if [ -x ./xkb_layout ]; then
                        cur_layout_nbr=$(./xkb_layout)
                        IFS=$',' read -r -a layouts < <(setxkbmap -query | grep layout | sed 's/layout:\s\+//g')
                        cur_layout="${layouts[$cur_layout_nbr]}"

                        echo "$TMUX_POWERLINE_SEG_XKB_LAYOUT_ICON $(echo "$cur_layout" | tr '[:lower:]' '[:upper:]')"
                    else
                        return 1
                    fi
                    ;;
                "wayland")
                    # Wayland has no global API to query the active keyboard layout
                    # from a non-focused client, so each compositor is queried via its
                    # own IPC and normalized to a short code below. Detection prefers
                    # exported env vars, then falls back to discovering the live socket
                    # under $XDG_RUNTIME_DIR (env vars are not always inherited by the
                    # long-lived tmux server).
                    local runtime_dir="${XDG_RUNTIME_DIR:-/run/user/$(id -u)}"
                    local layout_name=""

                    if [ -z "$SWAYSOCK" ]; then
                        for _sock in "$runtime_dir"/sway-ipc.*.sock; do
                            [ -S "$_sock" ] && SWAYSOCK="$_sock"
                        done
                    fi
                    if [ -z "$HYPRLAND_INSTANCE_SIGNATURE" ]; then
                        for _dir in "$runtime_dir"/hypr/*/; do
                            [ -d "$_dir" ] && HYPRLAND_INSTANCE_SIGNATURE="$(basename "$_dir")"
                        done
                    fi

                    if [ -n "$SWAYSOCK" ] && command -v swaymsg &>/dev/null; then
                        export SWAYSOCK
                        layout_name=$(swaymsg -t get_inputs 2>/dev/null |
                            jq -r 'first(.[] | select(.type=="keyboard") | .xkb_active_layout_name) // empty')
                    fi

                    if { [ -z "$layout_name" ] || [ "$layout_name" = "null" ]; } &&
                        [ -n "$HYPRLAND_INSTANCE_SIGNATURE" ] && command -v hyprctl &>/dev/null; then
                        export HYPRLAND_INSTANCE_SIGNATURE
                        layout_name=$(hyprctl devices -j 2>/dev/null |
                            jq -r '.keyboards[] | .active_keymap' | tail -n1)
                    fi

                    # dwl (and any compositor whose xkb patch writes the shared file).
                    if { [ -z "$layout_name" ] || [ "$layout_name" = "null" ]; } && [ -r /tmp/dwl-keymap ]; then
                        layout_name=$(cat /tmp/dwl-keymap)
                    fi

                    if [ -z "$layout_name" ] || [ "$layout_name" = "null" ]; then
                        return 1
                    fi

                    case "$layout_name" in
                        us | US | "English (US)" | English*) cur_layout="US" ;;
                        ua | UA | Ukrainian*) cur_layout="UA" ;;
                        *) cur_layout=$(echo "$layout_name" | cut -c1-2 | tr '[:lower:]' '[:upper:]') ;;
                    esac

                    echo "$TMUX_POWERLINE_SEG_XKB_LAYOUT_ICON $cur_layout"
                    ;;
                *)
                    return 1
                    ;;
            esac
            ;;
        *)
            return 1
            ;;
    esac
}

# run_segment() {
#     if ! shell_is_linux; then
#         return 1
#     fi
#
#     if [ "$XDG_SESSION_TYPE" != "x11" ]; then
#         return 1
#     fi
#
#     cd "$TMUX_POWERLINE_DIR_SEGMENTS" || return
#     if [ ! -x "xkb_layout" ]; then
#         make clean xkb_layout &>/dev/null
#     fi
#
#     if [ -x ./xkb_layout ]; then
#         cur_layout_nbr=$(./xkb_layout)
#         IFS=$',' read -r -a layouts < <(setxkbmap -query | grep layout | sed 's/layout:\s\+//g')
#         cur_layout="${layouts[$cur_layout_nbr]}"
#
#         echo "$TMUX_POWERLINE_SEG_XKB_LAYOUT_ICON $(echo "$cur_layout" | tr '[:lower:]' '[:upper:]')"
#     else
#         return 1
#     fi
# }