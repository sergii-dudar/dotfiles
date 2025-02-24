from qtile_extras import widget

from libqtile import qtile
from libqtile.lazy import lazy
from util import colors, vars
from util.util import to_mouse_callbacks, to_span
from util.vars import var
from widget.wcommon import decorations_round_left, decorations_round_right, text_widget_defaults

colors = colors.current()

mouse_callbacks = to_mouse_callbacks(
    right_click_cmd=var.run.volume_control
)
volume_dynamic_icon = widget.Volume(
    padding=0,
    fmt=" {}" + to_span(" ", None, 8),
    unmute_format='{volume}' + to_span("%", "#6272a4"),
    emoji=True,
    emoji_list=[' ', ' ', ' ', ' '],
    mute_foreground=colors.colors.color8,
    foreground=colors.colors.color10,
    **text_widget_defaults,
    **decorations_round_left,
    **mouse_callbacks
)
volume_percentage_level = widget.Volume(
    padding=0,
    fmt="{} ",
    mute_format=to_span(" ", None, 4) + "Mut",
    mute_foreground=colors.colors.color8,
    unmute_format='{volume:02.0f}' + to_span("%", "#6272a4"),
    foreground=colors.widget_foreground_color[0],
    **text_widget_defaults,
    **decorations_round_right,
    **mouse_callbacks
)
