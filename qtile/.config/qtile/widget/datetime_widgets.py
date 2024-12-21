from qtile_extras import widget

from util import colors
from util.util import to_span
from widget.wcommon import (
    decorations_round,
    icon_widget_defaults,
)

colors = colors.current()

clock = widget.Clock(
    format=to_span(" ", colors.colors.color6[0], 17) +
            to_span(" ", colors.colors.color6[0], 5) +
            to_span("%I:%M", colors.colors.color12[0]) +
            to_span(" ", colors.colors.color6[0], 5) +
            to_span("%p","#6272a4"),
    fontsize=22,
    foreground=colors.colors.color3,
    padding=15,
    **decorations_round,
    **icon_widget_defaults,
)

date = widget.Clock(
    format=
    to_span(" ", "#7c8377", 17) +
    to_span("%A, %B %d", "#6272a4"),
    fontsize=22,
    foreground=colors.colors.color3,
    padding=15,
    **decorations_round,
    **icon_widget_defaults,
)
