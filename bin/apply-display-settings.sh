if $( cat /proc/acpi/button/lid/*/state | grep -q closed )
then
    xrandr --output eDP-1 --off
fi

#if $( cat /proc/acpi/button/lid/LID0/state | grep -q open )
#then
#	xrandr --output DP-1-1 --mode 1920x1080 --pos 0x0 --rotate normal --output eDP-1 --mode 1920x1080 --pos 1920x0 --rotate normal
#else
#	xrandr --output eDP-1 --off
#fi
#xrandr --output HDMI-1 --primary --mode 3840x2160 --rotate normal --dpi 50

# must be the same as in .Xresources

xrdb -merge ~/.Xresources &
xrandr --dpi 112

# if working machine:
# Check if both HDMI-1 and HDMI-2 are connected
if xrandr | grep -q "HDMI-1 connected"; then
    if xrandr | grep -q "HDMI-2 connected"; then
        xrandr --output HDMI-2 --rotate right
    fi
fi

# if xrandr | grep -q "HDMI-1 connected" && xrandr | grep -q "HDMI-2 connected"; then
#     # Rotate HDMI-2 to portrait if HDMI-1 and HDMI-2 are connected
#     # xrandr --output HDMI-2 --rotate right --dpi 192
#
#     export XFT_DPI=112
#     xrandr --output HDMI-1 --dpi 112
#
#     export XFT_DPI=84
#     xrandr --output HDMI-2 --rotate right --dpi 84
# else
#     xrdb -merge ~/.Xresources &
#     xrandr --dpi 112
# fi