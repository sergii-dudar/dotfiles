from qtile_extras import widget

from util import colors, vars
from util.util import to_mouse_callbacks, to_span
from widget.wcommon import (
    decorations_round,
    icon_widget_defaults,
)

colors = colors.current()
clock_mouse_callbacks = to_mouse_callbacks(left_click_cmd=vars.run.gnome_clocks)
clock = widget.Clock(
    format=to_span(" ", colors.colors.color6[0], 17) +
            to_span(" ", colors.colors.color6[0], 5) +
            to_span("%I:%M", colors.widget_foreground_color[0]) +
            to_span(" ", colors.colors.color6[0], 5) +
            to_span("%p","#6272a4"),
    fontsize=22,
    foreground=colors.widget_foreground_color,
    padding=15,
    **decorations_round,
    **icon_widget_defaults,
    **clock_mouse_callbacks
)

date_mouse_callbacks = to_mouse_callbacks(left_click_cmd=vars.run.gnome_calendar)
date = widget.Clock(
    format=
    to_span(" ", "#7c8377", 17) +
    to_span(" ", colors.colors.color6[0], 5) +
    to_span("%a,", "#6272a4") +
    to_span(" ", colors.colors.color6[0], 5) +
    to_span("%b", "#6272a4") +
    to_span(" ", colors.colors.color6[0], 5) +
    to_span("%d", "#6272a4"),

    # to_span("%a, %b %d", "#6272a4"),
    # to_span("%A, %B %d", "#6272a4"),
    fontsize=22,
    foreground=colors.colors.color3,
    padding=15,
    **decorations_round,
    **icon_widget_defaults,
    **date_mouse_callbacks
)
