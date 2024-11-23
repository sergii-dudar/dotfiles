import colors

#import colors_dt
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
from modules.variables import (
    default_font,
    default_font_size,
    default_font_widget,
    default_font_widget_size,
)

colors, backgroundColor, foregroundColor, workspaceColor, chordColor = colors.catppuccin()

	# colors = [["#232634", "#232634"],  #background (dark grey) [0]
	# 		   ["#51576d", "#51576d"],  #light grey [1]
	# 		   ["#f2d5cf", "#f2d5cf"],  #foreground (white) [2]
	# 		   ["#8caaee", "#8caaee"],  #blue) [3]
	# 		   ["#b5bfe2", "#b5bfe2"],  #light blue [4]
	# 		   ["#a6d189", "#a6d189"],  #green [5]
	# 		   ["#ef9f76", "#ef9f76"],  #orange [6]
	# 		   ["#ef9f76", "#ef9f76"],  #orange [6]
	# 		   ["#f4b8e4", "#f4b8e4"],  #pink [7]
	# 		   ["#ca9ee6", "#ca9ee6"],  #purple [8]
	# 		   ['#e78284', '#e78284'],  #red [9]
	# 		   ["#e5c890", "#e5c890"]]  #yellow [10]
	#
	# backgroundColor = "#232634"
	# foregroundColor = "#c6d0f5"
	# workspaceColor = "#e5c890"
	# foregroundColorTwo = "#babbf1"

color_overlay1 = ["#7f849c", "#7f849c"]

widget_defaults = dict(
    font=default_font_widget,
    fontsize = default_font_widget_size,
    padding = 0,
    background=backgroundColor
)
extension_defaults = widget_defaults.copy()

# sep = widget.Sep(
#     linewidth=1,
#     paddog=15,
#     # foreground=foregroundColor,
#     background=backgroundColor,
#     text = "||"
# )
sep = widget.TextBox(
    text=" 󱋱 ",
    fontsize=default_font_widget_size,
    font=default_font_widget,
    foreground=color_overlay1,
    background=backgroundColor
)
space = widget.TextBox(
    text=" ",
    fontsize=default_font_widget_size,
    font=default_font_widget,
    foreground=color_overlay1,
    background=backgroundColor
)
spacer = widget.Spacer(
    background=backgroundColor
)
groupbox = widget.GroupBox(
    font=default_font_widget,
    fontsize=default_font_widget_size,
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
    foreground=foregroundColor,
    background=backgroundColor,
    use_mouse_wheel=False)

volicon = widget.TextBox(
    text="󰕾",
    fontsize=25,
    font=default_font_widget,
    foreground=colors[2],
    background=backgroundColor
)
volume = widget.Volume(
    padding=10,
    foreground=colors[2],
    background=backgroundColor
)
cpuicon = widget.TextBox(
    text=" ",
    fontsize=20,
    font=default_font_widget,
    foreground=colors[3],
    background=backgroundColor
)
cpu = widget.CPU(
    font=default_font_widget,
    update_interval=1.0,
    format="{load_percent}%",
    padding=5,
    foreground=colors[2],
    background=backgroundColor
)
memicon = widget.TextBox(
    text="",
    fontsize=20,
    font=default_font_widget,
    foreground=colors[6],
    background=backgroundColor
)
mem = widget.Memory(
    font=default_font_widget,
    foreground=foregroundColor,
    background=backgroundColor,
    format="{MemUsed: .0f}{mm} /{MemTotal: .0f}{mm}",
    measure_mem="G",
    padding=5,
)
clockicon = widget.TextBox(
    text="  ",
    fontsize=20,
    font=default_font_widget,
    foreground=colors[5],
    background=backgroundColor
)
clock = widget.Clock(
    format="%I:%M %p",
    font=default_font_widget,
    fontsize = default_font_widget_size,
    padding=10,
    foreground=colors[2],
    background=backgroundColor
)
curlayout = widget.CurrentLayoutIcon(
    scale=0.5,
    padding=10,
    foreground=foregroundColor,
    background=backgroundColor,
)
tray = widget.Systray(
    background=backgroundColor
)
windowname = widget.WindowName(
    font=default_font_widget,
    fontsize = default_font_widget_size,
    foreground=foregroundColor,
    background=backgroundColor,
)
keyboard = widget.KeyboardLayout(
    configured_keyboards=['us','ua'],
    font = default_font_widget,
    fontsize = default_font_widget_size,
    fmt = "  {}",
    foreground=foregroundColor,
    background=backgroundColor,
)

bar_widgers = [
    # left
    curlayout,
    sep,
    windowname,
    spacer,

    # center
    groupbox,
    sep,
    clockicon,
    clock,
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
    tray,
    space
    # sep,
    # sep,
]
