import os
import subprocess

from qtile_extras import widget
from qtile_extras.popup.templates.mpris2 import COMPACT_LAYOUT, DEFAULT_LAYOUT
from qtile_extras.widget.decorations import BorderDecoration, RectDecoration  # for decorations

from libqtile import qtile
from libqtile.lazy import lazy
from modules import (
    colors,
    variables,
    widgets_custom,
)
from modules.variables import (
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
decorations_no_round=dict(
    decorations=[
        RectDecoration(
            radius=[ 0, 0, 0, 0 ],
            **rect_decoraiton_defaults
        )
])
decorations_round=dict(
    decorations=[
        RectDecoration(
            radius=[ 17, 17, 17, 17 ],
            **rect_decoraiton_defaults
        )
])
decorations_round_left=dict(
    decorations=[
        RectDecoration(
            radius=[ 17, 0, 0, 17 ],
            **rect_decoraiton_defaults
        )
])
decorations_round_right=dict(
    decorations=[
        RectDecoration(
            radius=[ 0, 17, 17, 0 ],
            **rect_decoraiton_defaults
        )
])

sep = widget.TextBox(
    text="󱋱",
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
    **decorations_round_left
)
space_rec_right = widget.TextBox(
    text=" ",
    foreground=color_overlay1,
    **text_widget_defaults,
    **decorations_round_right
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
    #text="   <span color='" + colors[9][1] + "'></span>      <span color='" + colors[10][1] + "'> </span> ",
    text="  ",
    fontsize=18,
    foreground="#61afef",
    **applications_launcher,
    **icon_widget_defaults,
    **decorations_round
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
    fontsize=22,
    foreground=foregroundColor,
    **icon_widget_defaults
)

volume_dynamic_icon = widget.Volume(
    padding=0,
    fmt=" {} ",
    unmute_format='{volume}%',
    emoji=True,
    emoji_list=['', '', '', ''],
    mute_foreground="#d35f5e",
    foreground=colors[9],
    **text_widget_defaults,
    **decorations_round_left
)
volume_percentage_level = widget.Volume(
    padding=0,
    fmt="{} ",
    mute_format=" Mut",
    mute_foreground="#d35f5e",
    unmute_format=' {volume:2.0f}%',
    foreground=colors[2],
    **text_widget_defaults,
    **decorations_round_right
)
cpuicon = widget.TextBox(
    text="  ",
    fontsize=20,
    foreground=colors[3],
    **icon_widget_defaults,
    **decorations_round_left
)
cpu = widget.CPU(
    update_interval=1.0,
    format="{load_percent:2.0f}% ",
    foreground=colors[2],
    **text_widget_defaults,
    **decorations_round_right
)
memicon = widget.TextBox(
    text="  ",
    fontsize=20,
    foreground="#a6e3a1",
    **icon_widget_defaults,
    **decorations_round_left
)
mem = widget.Memory(
    #format="{MemUsed: .0f}{mm} /{MemTotal: .0f}{mm} ",
    format="{MemPercent:2.0f}% ",
    #mouse_callbacks = {'Button1': lambda: qtile.cmd_spawn(myTerm + ' -e htop')},
    measure_mem="G",
    foreground=foregroundColor,
    **text_widget_defaults,
    **decorations_round_right
)

clockicon_ext = widget.TextBox(
    text="  ",
    fontsize=22,
    foreground=colors[5],
    **decorations_round_left,
    **icon_widget_defaults
)

clock_ext = widgets_custom.MouseClickClock(
    format="%I:%M %p",
    fontsize=22,
    foreground=colors[2],
    padding=15,
    **decorations_round_right,
    **icon_widget_defaults,
)

curlayout = widget.CurrentLayoutIcon(
    scale=0.7,
    use_mask=True,
    foreground=colors[3],
    **icon_widget_defaults,
    **decorations_no_round
)
curlayoutText = widget.CurrentLayout(
    foreground=foregroundColor,
    fmt="[<i>{}</i>] ",
    **text_widget_defaults,
    **decorations_round_right
)
tray = widget.Systray(
    foreground=foregroundColor,
    icon_size=22,
    padding=12,
    **text_widget_defaults,
    **decorations_no_round
)
powermenu = widget.TextBox(
    text="  ",
    fontsize=20,
    foreground="#d35f5e",
    mouse_callbacks = dict(
        Button1=lambda: qtile.spawn(os.path.expanduser("~/dotfiles/bin/powermenu"))
    ),
    **icon_widget_defaults,
    **decorations_no_round
)
windowname = widget.WindowName(
    foreground=colors[8],
    **text_widget_defaults,
    # decorations=[
    #     BorderDecoration(
    #         colour = colors[5],
    #         border_width = [0, 0, 2, 0],
    #     )
    # ],
)

disc_icon = widget.TextBox(
    text="  ",
    fontsize=20,
    foreground=colors[11],
    **decorations_round_left,
    **icon_widget_defaults
)
disc_usage=widget.DF(
    # String format (p: partition, s: size, f: free space, uf: user free space, m: measure, r: ratio (uf/s))
    format = "{r:.0f}%",
    visible_on_warn=False,
    foreground=foregroundColor,
    **text_widget_defaults,
    **decorations_no_round,
)
disc_ssd_text = widget.TextBox(
    text="SSD ",
    fontsize=20,
    foreground=colors[11],
    **decorations_round_right,
    **icon_widget_defaults
)

keyboard_icon = widget.TextBox(
    text="  ",
    fontsize=20,
    foreground=colors[3],
    **icon_widget_defaults,
    **decorations_round_left
)
keyboard = widget.KeyboardLayout(
    configured_keyboards=['us','ua'],
    fmt = "{} ",
    foreground=foregroundColor,
    **text_widget_defaults,
    **decorations_round_right

)
arch_icon = widget.TextBox(
    text=" ❤ ",
    fontsize=25,
    foreground=colors[6],
    **applications_launcher,
    **icon_widget_defaults,
    **decorations_round_left
)
arch_version = widget.GenPollText(
    update_interval = 9999,
    func = lambda: subprocess.check_output("printf $(uname -r)", shell=True, text=True),
    fmt = '{} ',
    foreground=colors[3],
    **applications_launcher,
    **text_widget_defaults,
    **decorations_round_right
)

battery_icon = widget.TextBox(
    text="  ",
    fontsize=25,
    foreground=colors[5],
    **applications_launcher,
    **icon_widget_defaults,
    **decorations_round_left
)
battery = widget.Battery(format="{percent:2.0%} ",
    charge_char="⚡",
    discharge_char="🔋",
    full_char="⚡",
    unknown_char="⚡",
    empty_char="⁉️ ",
    update_interval=120,
    show_short_text=True,
    default_text="",
    **text_widget_defaults,
    **decorations_round_right
)
task_list = widget.TaskList(
    theme_path = "/usr/share/icons/Dracula",
    highlight_method='block',
    borderwidth=0,
    max_title_width=100,
    **icon_widget_defaults,
    icon_size=24,
    #theme_mode='fallback',
    stretch=False,
    padding=5,
    theme_mode = "preferred",
    fontsize=18,
    foreground=colors[3][1],
    #border=colors[1][1],
    border="#44475a"
)

music_control = widget.Mpris2(
    #format = "{xesam:title} - {xesam:artist}",
    foreground=colors[5],
    format = "{xesam:title}",
    max_chars = 30,
    paused_text = " ⏸️ {track} ",
    playing_text = " ▶️ {track} ",
    mouse_callbacks={
        "Button3": lazy.widget["mpris2"].toggle_player()
    },
    #popup_layout=COMPACT_LAYOUT
    popup_layout=DEFAULT_LAYOUT,
    popup_show_args=dict(
        relative_to=2,
        x=-1700,
        relative_to_bar=True,
        hide_on_timeout=5
    ),
    #width=200,
    scroll=False,
    **text_widget_defaults,
    **decorations_round
)

chord=widget.Chord(
    **text_widget_defaults,
    **decorations_round,
    foreground=colors[5],
    fmt=" <span color='" + colors[9][1] + "'></span> {} <span color='" + colors[9][1] + "'></span> "
)

bar_widgers = [
    # left
    applications,
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
    chord,
    keyboard_icon,
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
    space_rec_left,
    tray,
    powermenu,
    space_rec_right
]
