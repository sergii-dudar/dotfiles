from qtile_extras import widget

from libqtile import qtile
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
applications = widget.TextBox(
    #text="   <span color='" + colors[9][1] + "'></span>      <span color='" + colors[10][1] + "'> </span> ",
    text=" ",
    fontsize=21,
    foreground=colors.colors.color4,
    **applications_launcher,
    **icon_widget_defaults,
    **decorations_no_round
)

powermenu = widget.TextBox(
    text="  ",
    fontsize=22,
    foreground=colors.colors.color8,
    mouse_callbacks = dict(
        Button1=lambda: qtile.spawn(vars.powermenu)
    ),
    **icon_widget_defaults,
    **decorations_no_round
)
