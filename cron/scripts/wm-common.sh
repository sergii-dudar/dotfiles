# Function to detect window manager
# Check for common window managers by process name
if pgrep -x "Hyprland" > /dev/null; then
    WM="hyprland"
    HYPRLAND_INSTANCE_SIGNATURE=$(hyprctl instances -j | jq ".[].instance" -r | tail -n 1)

    # to enable hyprctl communications
    export HYPRLAND_INSTANCE_SIGNATURE
elif pgrep -x "sway" > /dev/null; then
    WM="sway"
elif pgrep -x "qtile" > /dev/null; then
    WM="qtile"
elif pgrep -x "i3" > /dev/null; then
    WM="i3"
elif pgrep -x "awesome" > /dev/null; then
    WM="awesomeWM"
else
    WM="Unknown"
fi

# to enable notifications
export DISPLAY=:0
export DBUS_SESSION_BUS_ADDRESS=unix:path=/run/user/$(id -u)/bus
