import os

from widget.datetime_widgets import clock, date
from widget.keyboard_widgets import keyboard
from widget.music_widgets import music_control
from widget.qtile_widgets import chord, curlayout, curlayoutText, groupbox, task_list, windowname
from widget.runner_widgets import applications, powermenu
from widget.simple_widgets import (
    arch_icon,
    arch_version,
    sep,
    space,
    space_rec_left,
    space_rec_right,
    spacer,
    to_space_rec_left,
    to_space_rec_right,
)
from widget.system_widgets import (
    battery,
    battery_icon,
    cpu,
    cpuicon,
    disc_icon,
    disc_ssd_text,
    disc_usage,
    mem,
    memicon,
    tray,
)
from widget.volume_widgets import volume_dynamic_icon, volume_percentage_level
from widget.weather_widgets import weather

bar_widgers = [
    # left
    to_space_rec_left(10),
    applications,
    to_space_rec_right(3),
    music_control,
    sep,
    space_rec_left,
    curlayout,
    curlayoutText,
    sep,
    task_list,
    #windowname,
    spacer,

    # center
    date,
    sep,
    sep,
    groupbox,
    sep,
    sep,
    # widget.TextBox(
    #     text=" 󱋱  ",
    #     foreground=color_overlay1,
    #     **text_widget_defaults
    # ),
    clock,
    spacer,

    # right
    chord,
    # keyboard_icon,
    keyboard,
    sep,
    volume_dynamic_icon,
    volume_percentage_level,
    sep
] + (
        # add battery modules only in case battery is present in the system
        # upower -e
        # upower -i /org/freedesktop/UPower/devices/battery_BAT0
        # /sys/class/power_supply/BAT0
        [battery_icon, battery, sep]
        if os.path.isdir("/sys/class/power_supply/BAT0")
        else []
    ) + [
    memicon,
    mem,
    sep,
    cpuicon,
    cpu,
    sep,
    disc_icon,
    disc_usage,
    disc_ssd_text,
    # sep,
    # arch_icon,
    # arch_version,
    sep,
    weather,
    sep,
    to_space_rec_left(12),
    tray,
    powermenu,
    to_space_rec_right(3)
]
