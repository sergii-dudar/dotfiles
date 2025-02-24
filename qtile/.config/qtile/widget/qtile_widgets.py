from qtile_extras import widget

from libqtile import bar, group, hook, layout, qtile, widget
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

ccolors = colors.current()
curlayout = widget.CurrentLayoutIcon(
    scale=0.7,
    use_mask=True,
    foreground=ccolors.colors.color4,
    **icon_widget_defaults,
    **decorations_no_round
)
class UppercaseCurrentLayout(widget.CurrentLayout):
    def hook_response(self, layout, group):
        if group.screen is not None and group.screen == self.bar.screen:
            self.text = layout.name.upper()
            self.bar.draw()

curlayoutText = UppercaseCurrentLayout(
    foreground=ccolors.widget_foreground_color,
    fmt="[<i>{}</i>] ",
    format_func=lambda text: text.upper(),
    **text_widget_defaults,
    **decorations_round_right
)
windowname = widget.WindowName(
    foreground=ccolors.colors.color9,
    **text_widget_defaults,
    # decorations=[
    #     BorderDecoration(
    #         colour = colors[5],
    #         border_width = [0, 0, 2, 0],
    #     )
    # ],
)

def build_groupbox(monitor_index: int):
    visible_groups_dict = dict()
    if len(qtile.screens) != 1:
        if monitor_index == 0:
            visible_groups_dict = dict(
                visible_groups=[ '1', '2', '3', '4', '5', '6', '7', '8' ]
            )
        else:
            visible_groups_dict = dict(
                visible_groups=[ '9' ]
            )

    return widget.GroupBox(
        margin_y=5,
        margin_x=0,
        padding_y=6,
        padding_x=6,
        disable_drag=True,
        active=ccolors.colors.color4, # unfocused
        inactive=ccolors.colors.color2,
        block_highlight_text_color=ccolors.colors.color6,
        hide_unused=False,
        highlight_method="line", # 'border', 'block', 'text', or 'line'
        borderwidth=5,
        rounded=False,
        highlight_color=["#44475a", "#44475a"],
        this_current_screen_border=ccolors.colors.color9, #color_overlay1,
        this_screen_border=ccolors.colors.color11,
        other_current_screen_border=ccolors.colors.color3,
        other_screen_border=ccolors.colors.color7,
        urgent_alert_method="line",
        urgent_border=ccolors.colors.color7,
        urgent_text=ccolors.colors.color2,
        use_mouse_wheel=False,
        spacing=0,
        center_aligned=True,
        fontsize=22,
        foreground=ccolors.foreground_color,
        **visible_groups_dict,
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
    foreground=ccolors.colors.color4[1],
    #border=colors[1][1],
    border="#44475a"
)
chord=widget.Chord(
    **text_widget_defaults,
    **decorations_round,
    foreground=ccolors.colors.color6,
    fmt=to_span(" ", ccolors.colors.color10[1])
        + " {} "
        + to_span(" ", ccolors.colors.color10[1])
)
