wm_name="${1:-}"

if $( bat /proc/acpi/button/lid/*/state | grep -q closed ); then
    case "$wm_name" in
        sway)
            swaymsg output eDP-1 disable
            ;;
        hyprland)
            hyprctl keyword monitor eDP-1,disable
            ;;
    esac
fi

# xrdb -merge ~/.Xresources
# xrandr --dpi 112

# if working machine:
# Check if both HDMI-1 and HDMI-2 are connected
# if xrandr | grep -q "HDMI-1 connected"; then
#     if xrandr | grep -q "HDMI-2 connected"; then
#         xrandr --output HDMI-2 --rotate right
#     fi
# fi

# LAPTOP_OUTPUT="<LAPTOP>"
# LID_STATE_FILE="/proc/acpi/button/lid/LID0/state"
#
# read -r LS < "$LID_STATE_FILE"
#
# case "$LS" in
# *open)   swaymsg output "$LAPTOP_OUTPUT" enable ;;
# *closed) swaymsg output "$LAPTOP_OUTPUT" disable ;;
# *)       echo "Could not get lid state" >&2 ; exit 1 ;;
# esac