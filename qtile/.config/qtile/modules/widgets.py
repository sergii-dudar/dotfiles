import colors
import colors_dt

from libqtile import bar, group, hook, layout, widget
from libqtile.config import (
    Click,
    Drag,
    DropDown,
    Group,
    Key,
    KeyChord,
    Match,
    Rule,
    ScratchPad,
    Screen,
)
from libqtile.lazy import lazy
from libqtile.log_utils import logger
from libqtile.utils import send_notification
from modules.variables import default_font, default_font_widget

colors, backgroundColor, foregroundColor, workspaceColor, chordColor = colors.dwm()

sep = widget.Sep(linewidth=1, paddog=15, foreground=colors[0], background=colors[0])
spacer = widget.Spacer(background=colors[11])
groupbox = widget.GroupBox(
    font="CaskaydiaCove Nerd Font",
    fontsize=20,
    margin_y=4,
    margin_x=4,
    padding_y=6,
    padding_x=6,
    borderwidth=2,
    disable_drag=True,
    active=colors[2], # unfocused
    inactive=colors[3],
    hide_unused=False,
    rounded=False,
    highlight_method="text",
    highlight_color=colors[0], # box color
    this_current_screen_border=colors[6],
    this_screen_border=colors[10],
    other_current_screen_border=colors[2],
    block_highlight_text_color=colors[6],
    other_screen_border=colors[6],
    urgent_alert_method="line",
    urgent_border=colors[6],
    urgent_text=colors[1],
    foreground=colors[0],
    background=colors[0],
    use_mouse_wheel=False)

# weather = widget.OpenWeather(
#     app_key="4cf3731a25d1d1f4e4a00207afd451a2",
#     cityid="4997193",
#     format="{icon} {main_temp}°",
#     metric=False,
#     font=default_font,
#     fontsize=13,
#     background=colors[0],
#     foreground=colors[2],
# )

volicon = widget.TextBox(text="󰕾", fontsize=25, font=default_font, foreground=colors[2], background=colors[0])
volume = widget.Volume(foreground=colors[2], padding=10, background=colors[0])
cpuicon = widget.TextBox(text="", fontsize=20, font=default_font, background=colors[0], foreground=colors[3])
cpu = widget.CPU(font=default_font_widget, update_interval=1.0, format="{load_percent}%", foreground=colors[2], background=colors[0], padding=5)
memicon = widget.TextBox(text="", fontsize=20, font=default_font, background=colors[0], foreground=colors[6])
mem = widget.Memory(
    font=default_font_widget,
    foreground=colors[2],
    background=colors[0],
    format="{MemUsed: .0f}{mm} /{MemTotal: .0f}{mm}",
    measure_mem="G",
    padding=5,
)
clockicon = widget.TextBox(text="", fontsize=20, font=default_font, background=colors[0], foreground=colors[5])
clock = widget.Clock(format="%I:%M %p", font=default_font_widget, padding=10, background=colors[0], foreground=colors[2])
curlayout = widget.CurrentLayoutIcon(
    scale=0.5,
    foreground=colors[0],
    background=colors[11],
    padding=10,
)
tray = widget.Systray(background=colors[0])
windowname = widget.WindowName(
    font=default_font_widget,
    foreground=colors[0],
    background=colors[6]
)
keyboard = widget.KeyboardLayout(
    configured_keyboards=['us','ua'],
    font = default_font_widget,
    fontsize = 18,
    fmt = "  {}"
)

bar_widgers = [
    # left
    sep,
    curlayout,
    windowname,
    spacer,

    # center
    groupbox,
    clockicon,
    clock,
    spacer,

    # right
    #keyboard,
    keyboard,
    sep,
    volicon,
    volume,
    cpuicon,
    cpu,
    memicon,
    mem,
    sep,
    sep,
    sep,
    tray,
    sep,
    sep,
]
