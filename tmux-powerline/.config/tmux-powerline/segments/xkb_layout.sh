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
                    # Wayland has no compositor-agnostic layout API; the per-compositor
                    # dispatch lives in xkb_layout_wayland.sh and prints a short code.
                    cur_layout=$("$HOME/.config/tmux-powerline/segments/xkb_layout_wayland.sh") || return 1
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