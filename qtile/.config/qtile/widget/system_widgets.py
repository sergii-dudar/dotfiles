import logging

from qtile_extras import widget

from libqtile import qtile
from libqtile.lazy import lazy
from libqtile.log_utils import logger
from util import colors, vars
from util.util import to_mouse_callbacks, to_span
from util.vars import var
from widget.wcommon import (
    decorations_no_round,
    decorations_round_left,
    decorations_round_right,
    icon_widget_defaults,
    text_widget_defaults,
)

colors = colors.current()

cpu_mouse_callbacks=to_mouse_callbacks(
    left_click_cmd=var.run.gnome_system_monitor
)
cpuicon = widget.TextBox(
    text="  ",
    fontsize=23,
    foreground=colors.colors.color4,
    **icon_widget_defaults,
    **decorations_round_left,
    **cpu_mouse_callbacks
)
cpu = widget.CPU(
    update_interval=1.0,
    padding=0,
    format="{load_percent:02.0f}" + to_span("% ", "#6272a4"),
    foreground=colors.widget_foreground_color[0],
    **text_widget_defaults,
    **decorations_round_right,
    **cpu_mouse_callbacks
)

temperature_icon = widget.TextBox(
    text=" ",
    fontsize=23,
    foreground="#b4befe",
    padding=6,
    **icon_widget_defaults,
    **decorations_round_left,
    **cpu_mouse_callbacks
)

# required deps: sensors
temperature = widget.ThermalSensor(
    update_interval=10,
    padding=0,
    format="{temp:.0f}" + to_span("°C ", "#6272a4"),
    fgcolor_high=colors.widget_foreground_color[0],
    fgcolor_normal=colors.widget_foreground_color[0],
    foreground=colors.widget_foreground_color[0],
    tag_sensor="Core 0",
    **text_widget_defaults,
    **decorations_round_right,
    **cpu_mouse_callbacks
)
# temperature = widget.ThermalZone(
#     update_interval=10,
#     padding=0,
#     format="{temp}" + to_span("°C ", "#6272a4"),
#     fgcolor_high=colors.widget_foreground_color[0],
#     fgcolor_normal=colors.widget_foreground_color[0],
#     foreground=colors.widget_foreground_color[0],
#     **text_widget_defaults,
#     **decorations_round_right,
#     **cpu_mouse_callbacks
# )

mem_mouse_callbacks=to_mouse_callbacks(
    left_click_cmd=var.run.htop
)
memicon = widget.TextBox(
    text="  ",
    fontsize=22,
    foreground=colors.colors.color6,
    **icon_widget_defaults,
    **decorations_round_left,
    **mem_mouse_callbacks
)
mem = widget.Memory(
    #fmt = '{}',
    #format="{MemUsed: .0f}{mm} /{MemTotal: .0f}{mm} ",
    format="{MemPercent:02.0f}" + to_span("% ", "#6272a4"),
    #mouse_callbacks = {'Button1': lambda: qtile.cmd_spawn(myTerm + ' -e htop')},
    measure_mem="G",
    foreground=colors.widget_foreground_color[0],
    padding=0,
    **text_widget_defaults,
    **decorations_round_right,
    **mem_mouse_callbacks
)

disc_mouse_callbacks=to_mouse_callbacks(
    left_click_cmd=var.run.disc_gdu
)
disc_icon = widget.TextBox(
    text="  ",
    fontsize=22,
    foreground=colors.colors.color12,
    **decorations_round_left,
    **icon_widget_defaults,
    **disc_mouse_callbacks
)
disc_usage=widget.DF(
    # String format (p: partition, s: size, f: free space, uf: user free space, m: measure, r: ratio (uf/s))
    padding=2,
    format = "{r:.0f}" + to_span("%", "#6272a4"),
    visible_on_warn=False,
    foreground=colors.widget_foreground_color[0],
    **text_widget_defaults,
    **decorations_no_round,
    **disc_mouse_callbacks
)
disc_ssd_text = widget.TextBox(
    text="SSD ",
    foreground=colors.colors.color4,
    **decorations_round_right,
    **text_widget_defaults,
    **disc_mouse_callbacks
)

battery_icon = widget.TextBox(
    text="  ",
    fontsize=25,
    foreground=colors.colors.color6,
    **icon_widget_defaults,
    **decorations_round_left
)

class BatteryCustom(widget.Battery):
    def __init__(self, **config):
        super().__init__(**config)
    def poll(self):
        # widget.Battery adding percent symbol that in result cant be customised, remove it, to be able to place customised
        return super().poll().replace("%<span", "<span")

battery = BatteryCustom(format="{percent:2.0%}" + to_span("% ", "#6272a4"),
    charge_char="⚡",
    discharge_char="🔋",
    full_char="⚡",
    unknown_char="⚡",
    empty_char="⁉️ ",
    update_interval=120,
    show_short_text=True,
    default_text="",
    foreground=colors.widget_foreground_color[0],
    **text_widget_defaults,
    **decorations_round_right
)

tray = widget.Systray(
    foreground=colors.foreground_color,
    icon_size=27,
    padding=10,
    **text_widget_defaults,
    **decorations_no_round
)
