import os
import subprocess

from qtile_extras import widget

from libqtile import qtile
from libqtile.lazy import lazy
from util import colors, vars
from util.util import to_span
from util.vars import var
from widget.wcommon import (
    applications_launcher,
    decorations_no_round,
    decorations_round_left,
    decorations_round_right,
    icon_widget_defaults,
    text_widget_defaults,
)

colors = colors.current()
overlay = colors.overlay

arch_icon = widget.TextBox(
    text=" ❤ ",
    fontsize=25,
    foreground=colors.colors.color7,
    **applications_launcher,
    **icon_widget_defaults,
    **decorations_round_left
)
arch_version = widget.GenPollText(
    update_interval = 9999,
    func = lambda: subprocess.check_output("printf $(uname -r)", shell=True, text=True),
    fmt = '{} ',
    foreground=colors.colors.color4,
    **applications_launcher,
    **text_widget_defaults,
    **decorations_round_right
)

# sep = widget.TextBox(
#     text="󱋱",
#     foreground=color_overlay1,
#     **text_widget_defaults
# )
sep = widget.TextBox(
    text=" ",
    foreground=colors.overlay,
    fontsize = 0,
    **icon_widget_defaults,
)
space = widget.TextBox(
    text=" ",
    foreground=colors.overlay,
    **text_widget_defaults
)
space_rec_left = widget.TextBox(
    text=" ",
    foreground=colors.overlay,
    **text_widget_defaults,
    **decorations_round_left
)
space_rec_right = widget.TextBox(
    text=" ",
    foreground=colors.overlay,
    **text_widget_defaults,
    **decorations_round_right
)

spacer = widget.Spacer(
    **text_widget_defaults
)

def to_space_rec_right(size: float):
    return widget.TextBox(
        text=" ",
        fontsize = size,
        foreground=overlay,
        **icon_widget_defaults,
        **decorations_round_right
    )

def to_space_rec_left(size: float):
    return widget.TextBox(
        text=" ",
        fontsize = size,
        foreground=overlay,
        **icon_widget_defaults,
        **decorations_round_left
    )
