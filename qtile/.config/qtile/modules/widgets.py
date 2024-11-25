#import colors_dt
import subprocess

from libqtile import bar, group, hook, layout, widget

#from qtile_extras import widget
#from qtile_extras.widget.decorations import RectDecoration  # for decorations
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
    margin_y=5,
    margin_x=5,
    padding_y=6,
    padding_x=6,
    #padding=6, #[ 0, 0, 7, 0 ],
    disable_drag=True,
    active=colors[3], # unfocused
    inactive=colors[1],
    block_highlight_text_color=colors[5],
    hide_unused=False,
    #highlight_method="text", # 'border', 'block', 'text', or 'line'
    highlight_method="line", # 'border', 'block', 'text', or 'line'
    borderwidth=5,
    rounded=False,
    highlight_color=["#44475a", "#44475a"], #["#282a36", "#282a36"],#colors[1], # box color
    this_current_screen_border=colors[8], #color_overlay1,
    this_screen_border=colors[10],
    other_current_screen_border=colors[2],
    other_screen_border=colors[6],
    urgent_alert_method="line",
    urgent_border=colors[6],
    urgent_text=colors[1],
    foreground=foregroundColor,
    background=backgroundColor,
    use_mouse_wheel=False,
    spacing=0,
    center_aligned=True,
)

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
    background=backgroundColor,
    font=default_font_widget,
    fontsize=default_font_widget_size,
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
    background=backgroundColor,
    fontsize=default_font_widget_size,
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
    fontsize=default_font_widget_size,
    format="{MemUsed: .0f}{mm} /{MemTotal: .0f}{mm}",
    #mouse_callbacks = {'Button1': lambda: qtile.cmd_spawn(myTerm + ' -e htop')},
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

@lazy.function
def toggle_clock_format(qtile):
    cwidget = next(w for w in qtile.widgets_map.values() if isinstance(w, widget.Clock))
    if cwidget.format == "%I:%M %p":  # Current format is clock
        cwidget.format = "%Y-%m-%d"   # Switch to date
    else:
       cwidget.format = "%I:%M %p"   # Switch back to clock
    cwidget.tick()  # Force the widget to refresh immediately

clock = widget.Clock(
    format="%I:%M %p",
    #format = "%a, %b %d - %I:%M %p",
    font=default_font_widget,
    fontsize = default_font_widget_size,
    padding=10,
    foreground=colors[2],
    background=backgroundColor,
    mouse_callbacks={"Button1": toggle_clock_format},
)

class MouseOverClock(widget.Clock):
    # defaults = [
    #     (
    #         "long_format",
    #         "%A %d %B %Y | %H:%M",
    #         "Format to show when mouse is over widget."
    #     ),
    #     (
    #         "date",
    #         "%a, %b %d",
    #         "Format to show on click."
    #     )
    # ]
    #
    # def __init__(self, **config):
    #     widget.Clock.__init__(self, **config)
    #     self.add_defaults(MouseOverClock.defaults)
    #     self.short_format = self.format
    #
    # def mouse_enter(self, *args, **kwargs):
    #     self.format = self.long_format
    #     self.bar.draw()
    #
    # def mouse_leave(self, *args, **kwargs):
    #     self.format = self.short_format
    #     self.bar.draw()

    def __init__(self, **config):
        widget.Clock.__init__(self, **config)
        self.formats = ["%I:%M %p", "%A, %B %d"]
        self.current_format_index = 0
        self.format = self.formats[self.current_format_index]
        self.add_callbacks({
                "Button1": self.button_press,
            }
        )

    def button_press(self, x, y, button):
        self.current_format_index = (self.current_format_index + 1) % len(self.formats)
        self.format = self.formats[self.current_format_index]
        self.bar.draw()

clock_cust = MouseOverClock(
    format="%I:%M %p",
    #format = "%a, %b %d - %I:%M %p",
    font=default_font_widget,
    fontsize = default_font_widget_size,
    padding=10,
    foreground=colors[2],
    background=backgroundColor,
    mouse_callbacks={"Button1": toggle_clock_format},
)

curlayout = widget.CurrentLayoutIcon(
    scale=0.5,
    padding=10,
    foreground=foregroundColor,
    background=backgroundColor,
    fontsize=default_font_widget_size,
)
curlayoutText = widget.CurrentLayout(
    foreground=foregroundColor,
    background=backgroundColor,
    font=default_font_widget,
    fontsize = default_font_widget_size,
    padding = 5
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
arch_icon = widget.TextBox(
    text="❤ ",
    fontsize=25,
    font=default_font_widget,
    foreground=colors[6],
    background=backgroundColor
)
arch_version = widget.GenPollText(
    update_interval = 9999,
    func = lambda: subprocess.check_output("printf $(uname -r)", shell=True, text=True),
    fmt = '{}',
    font=default_font_widget,
    fontsize = default_font_widget_size,
    foreground=colors[3],
    background=backgroundColor,
)

bar_widgers = [
        # widget.Image(
        #          filename = "~/.config/qtile/Assets/logo.png",
        #          scale = "False",
        #          mouse_callbacks = {'Button1': lambda: qtile.cmd_spawn(myTerm)},
        #          ),


    # left
    curlayout,
    curlayoutText,
    sep,
    windowname,
    spacer,


    # center
    groupbox,
    sep,
    clockicon,
    clock,
    spacer,
    clock_cust,

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
    sep,
    arch_icon,
    arch_version,
    space
    # sep,
    # sep,
]
