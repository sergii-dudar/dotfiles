from qtile_extras import widget

from util import colors
from util.util import to_span
from util.vars import var
from widget.wcommon import (
    decorations_no_round,
    decorations_round,
    decorations_round_right,
    icon_widget_defaults,
    text_widget_defaults,
)

colors = colors.current()
curlayout = widget.CurrentLayoutIcon(
    scale=0.7,
    use_mask=True,
    foreground=colors.colors.color4,
    **icon_widget_defaults,
    **decorations_no_round
)
curlayoutText = widget.CurrentLayout(
    foreground=colors.foreground_color,
    fmt="[<i>{}</i>] ",
    format_func=lambda text: text.upper(),
    **text_widget_defaults,
    **decorations_round_right
)
windowname = widget.WindowName(
    foreground=colors.colors.color9,
    **text_widget_defaults,
    # decorations=[
    #     BorderDecoration(
    #         colour = colors[5],
    #         border_width = [0, 0, 2, 0],
    #     )
    # ],
)
groupbox = widget.GroupBox(
    margin_y=5,
    margin_x=0,
    padding_y=6,
    padding_x=6,
    disable_drag=True,
    active=colors.colors.color4, # unfocused
    inactive=colors.colors.color2,
    block_highlight_text_color=colors.colors.color6,
    hide_unused=False,
    highlight_method="line", # 'border', 'block', 'text', or 'line'
    borderwidth=5,
    rounded=False,
    highlight_color=["#44475a", "#44475a"],
    this_current_screen_border=colors.colors.color9, #color_overlay1,
    this_screen_border=colors.colors.color11,
    other_current_screen_border=colors.colors.color3,
    other_screen_border=colors.colors.color7,
    urgent_alert_method="line",
    urgent_border=colors.colors.color7,
    urgent_text=colors.colors.color2,
    use_mouse_wheel=False,
    spacing=0,
    center_aligned=True,
    fontsize=22,
    foreground=colors.foreground_color,
    **icon_widget_defaults
)
task_list = widget.TaskList(
    theme_path = "/usr/share/icons/Dracula",
    highlight_method='block',
    borderwidth=0,
    max_title_width=100,
    **icon_widget_defaults,
    icon_size=24,
    #theme_mode='fallback',
    stretch=False,
    padding=5,
    theme_mode = "preferred",
    fontsize=18,
    foreground=colors.colors.color4[1],
    #border=colors[1][1],
    border="#44475a"
)
chord=widget.Chord(
    **text_widget_defaults,
    **decorations_round,
    foreground=colors.colors.color6,
    fmt=to_span(" ", colors.colors.color10[1])
        + " {} "
        + to_span(" ", colors.colors.color10[1])
)
