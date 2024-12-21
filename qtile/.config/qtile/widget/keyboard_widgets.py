import os
import subprocess

from qtile_extras import widget
from qtile_extras.popup.templates.mpris2 import COMPACT_LAYOUT, DEFAULT_LAYOUT
from qtile_extras.widget.decorations import BorderDecoration, RectDecoration  # for decorations

from libqtile import qtile
from libqtile.lazy import lazy
from util import colors
from util.util import to_span
from widget.wcommon import (
    applications_launcher,
    decorations_round,
    decorations_round_left,
    decorations_round_right,
    icon_widget_defaults,
    text_widget_defaults,
)

colors = colors.current()
# keyboard_icon = widget.TextBox(
#     text="  ",
#     fontsize=20,
#     foreground=colors.colors.color4,
#     **icon_widget_defaults,
#     **decorations_round_left
# )
keyboard = widget.KeyboardLayout(
    configured_keyboards=['us','ua'],
    display_map ={
        "us": " 🇺🇸" + to_span(" ", None, 6) + "US",
        "ua": " 🇺🇦" + to_span(" ", None, 6) + "UA"
    },
    fmt = "{} ",
    foreground=colors.widget_foreground_color[0],
    **text_widget_defaults,
    **decorations_round
)