import os

from widget import runner_widgets
from widget.datetime_widgets import clock, date
from widget.keyboard_widgets import keyboard
from widget.music_widgets import music_control
from widget.qtile_widgets import chord, curlayout, curlayoutText, groupbox, task_list, windowname
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


def build_main_bar_widgets():
    return [
        # left
        to_space_rec_left(10),

        runner_widgets.applications,
        runner_widgets.settings,
        runner_widgets.ghostty_terminal,
        runner_widgets.kitty_runner,
        runner_widgets.wezterm_terminal,
        runner_widgets.pipette,
        # runner_widgets.chrome,
        runner_widgets.brave,
        runner_widgets.intellij,
        runner_widgets.insomnia,
        runner_widgets.torrent,

        to_space_rec_right(3),
        sep,
        space_rec_left,
        curlayout,
        curlayoutText,
        sep,
        music_control,
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
        to_space_rec_left(0),
        tray,
        runner_widgets.powermenu,
        to_space_rec_right(3)
    ]

def build_second_bar_widgets():
    return [
        to_space_rec_right(3),
        sep,
        space_rec_left,
        curlayout,
        curlayoutText,
        sep,
        task_list,

        spacer,
        groupbox,
        spacer,

        runner_widgets.ghostty_terminal,
        runner_widgets.kitty_runner,
        runner_widgets.wezterm_terminal,
        runner_widgets.pipette,
        # runner_widgets.chrome,
        runner_widgets.brave,
        runner_widgets.intellij,
        runner_widgets.insomnia,
        runner_widgets.torrent,
        runner_widgets.settings,
        runner_widgets.applications,

        to_space_rec_right(3),
    ]
