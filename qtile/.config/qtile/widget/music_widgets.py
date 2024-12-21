import os
import subprocess

from qtile_extras import widget
from qtile_extras.popup.templates.mpris2 import COMPACT_LAYOUT, DEFAULT_LAYOUT
from qtile_extras.widget.decorations import BorderDecoration, RectDecoration  # for decorations

from libqtile import qtile
from libqtile.lazy import lazy
from util import colors, vars
from util.util import to_span
from util.vars import (
    default_font_widget,
    default_font_widget_size,
)
from widget.wcommon import (
    applications_launcher,
    decorations_no_round,
    decorations_round,
    decorations_round_left,
    decorations_round_right,
    icon_widget_defaults,
    text_widget_defaults,
)

colors = colors.current()
music_control = widget.Mpris2(
    #format = "{xesam:title} - {xesam:artist}",
    foreground=colors.colors.color6,
    format = " {xesam:title} ",
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