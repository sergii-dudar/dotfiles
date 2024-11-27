import os
import subprocess

from qtile_extras import widget
from qtile_extras.widget.decorations import BorderDecoration, RectDecoration  # for decorations

#from libqtile import bar, group, hook, layout, widget
from libqtile import qtile
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
from modules import (
    colors,
    colors_dt,
    funcs,
    keybind,
    scratchpad,
    variables,
    widgets,
    widgets_custom,
    winrules,
    workspaces,
)
from modules.variables import (
    default_font,
    default_font_size,
    default_font_widget,
    default_font_widget_size,
)

colors, backgroundColor, foregroundColor, workspaceColor, chordColor = colors.catppuccin()
color_overlay1 = ["#7f849c", "#7f849c"]

text_widget_defaults = dict(
    font=default_font_widget,
    fontsize = default_font_widget_size,
    background=backgroundColor
)
icon_widget_defaults = dict(
    font=default_font_widget,
    background=backgroundColor
)
rect_decoraiton_defaults=dict(
    colour="#2b2f37",
    filled=True,
    padding_y=0,
)
dec_radius_left=dict(
   radius=[ 17, 0, 0, 17 ],
)
dec_radius_right=dict(
   radius=[ 0, 17, 17, 0 ],
)

sep = widget.TextBox(
    text=" 󱋱 ",
    foreground=color_overlay1,
    **text_widget_defaults
)
space = widget.TextBox(
    text=" ",
    foreground=color_overlay1,
    **text_widget_defaults
)
space_rec_left = widget.TextBox(
    text=" ",
    foreground=color_overlay1,
    **text_widget_defaults,
    decorations=[
        RectDecoration(
            **dec_radius_left,
            **rect_decoraiton_defaults
        )
    ],
)
space_rec_right = widget.TextBox(
    text=" ",
    foreground=color_overlay1,
    **text_widget_defaults,
        decorations=[
        RectDecoration(
            **dec_radius_right,
            **rect_decoraiton_defaults
        )
    ],
)
spacer = widget.Spacer(
    **text_widget_defaults
)
applications_launcher=dict(
    mouse_callbacks = dict(
        Button1=lambda: qtile.spawn(variables.mymenu),
    ),
)
applications = widget.TextBox(
    text="           ",
    fontsize=20,
    foreground="#61afef",
    **applications_launcher,
    **icon_widget_defaults,
    decorations=[
        RectDecoration(
            radius=[ 17, 17, 17, 17 ],
            **rect_decoraiton_defaults
        )
    ],
)

groupbox = widget.GroupBox(
    margin_y=5,
    margin_x=0,
    padding_y=6,
    padding_x=6,
    disable_drag=True,
    active=colors[3], # unfocused
    inactive=colors[1],
    block_highlight_text_color=colors[5],
    hide_unused=False,
    highlight_method="line", # 'border', 'block', 'text', or 'line'
    borderwidth=5,
    rounded=False,
    highlight_color=["#44475a", "#44475a"],
    this_current_screen_border=colors[8], #color_overlay1,
    this_screen_border=colors[10],
    other_current_screen_border=colors[2],
    other_screen_border=colors[6],
    urgent_alert_method="line",
    urgent_border=colors[6],
    urgent_text=colors[1],
    use_mouse_wheel=False,
    spacing=0,
    center_aligned=True,
    foreground=foregroundColor,
    **text_widget_defaults
)

volicon = widget.TextBox(
    text="󰕾 ",
    fontsize=25,
    foreground=colors[2],
    **icon_widget_defaults
)
volume = widget.Volume(
    padding=0,
    foreground=colors[2],
    **text_widget_defaults
)
cpuicon = widget.TextBox(
    text=" ",
    fontsize=20,
    foreground=colors[3],
    **icon_widget_defaults
)
cpu = widget.CPU(
    update_interval=1.0,
    format="{load_percent}%",
    foreground=colors[2],
    **text_widget_defaults
)
memicon = widget.TextBox(
    text="",
    fontsize=20,
    foreground=colors[6],
    **icon_widget_defaults
)
mem = widget.Memory(
    format="{MemUsed: .0f}{mm} /{MemTotal: .0f}{mm}",
    #mouse_callbacks = {'Button1': lambda: qtile.cmd_spawn(myTerm + ' -e htop')},
    measure_mem="G",
    foreground=foregroundColor,
    **text_widget_defaults
)

clockicon_ext = widget.TextBox(
    text="  ",
    fontsize=20,
    foreground=colors[5],
    decorations=[
        RectDecoration(
            **dec_radius_left,
            **rect_decoraiton_defaults
        )
    ],
    **icon_widget_defaults
)

clock_ext = widgets_custom.MouseClickClock(
    format="%I:%M %p",
    foreground=colors[2],
    padding=15,
    decorations=[
        RectDecoration(
            **dec_radius_right,
            **rect_decoraiton_defaults
        )
    ],
    **text_widget_defaults,
)

curlayout = widget.CurrentLayoutIcon(
    scale=0.6,
    **icon_widget_defaults,
)
curlayoutText = widget.CurrentLayout(
    foreground=foregroundColor,
    **text_widget_defaults
)
tray = widget.Systray(
    foreground=foregroundColor,
    **text_widget_defaults,
    decorations=[
        RectDecoration(
            radius=[ 0, 0, 0, 0 ],
            **rect_decoraiton_defaults
        )
    ],
)
powermenu = widget.TextBox(
    text="  ",
    fontsize=20,
    foreground="#d35f5e",
    mouse_callbacks = dict(
        Button1=lambda: qtile.spawn(os.path.expanduser("~/dotfiles/bin/powermenu"))
    ),
    **icon_widget_defaults,
    decorations=[
        RectDecoration(
            radius=[ 0, 0, 0, 0 ],
            **rect_decoraiton_defaults
        )
    ],

)
windowname = widget.WindowName(
    foreground=foregroundColor,
    **text_widget_defaults
)
keyboard = widget.KeyboardLayout(
    configured_keyboards=['us','ua'],
    fmt = "  {}",
    foreground=foregroundColor,
    **text_widget_defaults,
    # decorations=[
    #     BorderDecoration(
    #         colour = colors[5],
    #         border_width = [0, 0, 2, 0],
    #     )
    # ],
)
arch_icon = widget.TextBox(
    text=" ❤ ",
    fontsize=25,
    foreground=colors[6],
    **applications_launcher,
    **icon_widget_defaults,
    decorations=[
        RectDecoration(
            **dec_radius_left,
            **rect_decoraiton_defaults
        )
    ],
)
arch_version = widget.GenPollText(
    update_interval = 9999,
    func = lambda: subprocess.check_output("printf $(uname -r)", shell=True, text=True),
    fmt = '{} ',
    foreground=colors[3],
    **applications_launcher,
    **text_widget_defaults,
    decorations=[
        RectDecoration(
            **dec_radius_right,
            **rect_decoraiton_defaults
        )
    ],
)

bar_widgers = [
    # left
    applications,
    sep,
    curlayout,
    widget.TextBox(
        text="",
        foreground=color_overlay1,
        **text_widget_defaults
    ),
    curlayoutText,
    sep,
    windowname,
    spacer,


    # center
    groupbox,
    widget.TextBox(
        text=" 󱋱  ",
        foreground=color_overlay1,
        **text_widget_defaults
    ),
    clockicon_ext,
    clock_ext,
    spacer,

    # right
    #keyboard,
    keyboard,
    sep,
    volicon,
    volume,
    sep,
    cpuicon,
    cpu,
    sep,
    memicon,
    mem,
    sep,
    arch_icon,
    arch_version,
    sep,
    space_rec_left,
    tray,
    powermenu,
    space_rec_right
]
