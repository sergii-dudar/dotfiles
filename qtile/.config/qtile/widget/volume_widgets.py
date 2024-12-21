from qtile_extras import widget

from libqtile import qtile
from libqtile.lazy import lazy
from util import colors, vars
from util.util import to_span
from util.vars import var
from widget.wcommon import decorations_round_left, decorations_round_right, text_widget_defaults

colors = colors.current()
volume_dynamic_icon = widget.Volume(
    padding=0,
    fmt=" {}" + to_span(" ", None, 10),
    unmute_format='{volume}%',
    emoji=True,
    emoji_list=['', '', ' ', ' '],
    mute_foreground=colors.colors.color8,
    foreground=colors.colors.color10,
    **text_widget_defaults,
    **decorations_round_left
)
volume_percentage_level = widget.Volume(
    padding=0,
    fmt="{} ",
    mute_format=" Mut",
    mute_foreground=colors.colors.color8,
    unmute_format='{volume:02.0f}%',
    foreground=colors.colors.color3,
    **text_widget_defaults,
    **decorations_round_right
)
