from qtile_extras import widget

from libqtile import qtile
from libqtile.lazy import lazy
from util import colors, vars
from util.util import to_span
from widget.wcommon import (
    decorations_no_round,
    decorations_round_left,
    decorations_round_right,
    icon_widget_defaults,
    text_widget_defaults,
)

colors = colors.current()
cpuicon = widget.TextBox(
    text="  ",
    fontsize=20,
    foreground=colors.colors.color4,
    **icon_widget_defaults,
    **decorations_round_left
)
cpu = widget.CPU(
    update_interval=1.0,
    padding=0,
    format="{load_percent:02.0f}% ",
    foreground=colors.colors.color3,
    **text_widget_defaults,
    **decorations_round_right
)
memicon = widget.TextBox(
    text="  ",
    fontsize=20,
    foreground=colors.colors.color6,
    **icon_widget_defaults,
    **decorations_round_left
)
mem = widget.Memory(
    #fmt = '{}',
    #format="{MemUsed: .0f}{mm} /{MemTotal: .0f}{mm} ",
    format="{MemPercent:02.0f}% ",
    #mouse_callbacks = {'Button1': lambda: qtile.cmd_spawn(myTerm + ' -e htop')},
    measure_mem="G",
    foreground=colors.foreground_color,
    padding=0,
    **text_widget_defaults,
    **decorations_round_right
)
disc_icon = widget.TextBox(
    text="  ",
    fontsize=20,
    foreground=colors.colors.color12,
    **decorations_round_left,
    **icon_widget_defaults
)
disc_usage=widget.DF(
    # String format (p: partition, s: size, f: free space, uf: user free space, m: measure, r: ratio (uf/s))
    padding=2,
    format = "{r:.0f}%",
    visible_on_warn=False,
    foreground=colors.foreground_color,
    **text_widget_defaults,
    **decorations_no_round,
)
disc_ssd_text = widget.TextBox(
    text="SSD ",
    foreground=colors.colors.color12,
    **decorations_round_right,
    **text_widget_defaults
)
battery_icon = widget.TextBox(
    text="  ",
    fontsize=25,
    foreground=colors.colors.color6,
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
tray = widget.Systray(
    foreground=colors.foreground_color,
    icon_size=22,
    #padding=12,
    **text_widget_defaults,
    **decorations_no_round
)
