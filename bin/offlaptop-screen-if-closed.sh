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
