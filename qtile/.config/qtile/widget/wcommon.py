from qtile_extras.widget.decorations import BorderDecoration, RectDecoration  # for decorations

from libqtile import qtile
from util import colors
from util.vars import var

colors = colors.current()
text_widget_defaults = dict(
    font=var.font.default_font_widget,
    fontsize = var.font.default_font_widget_size,
    background=colors.background_color
)
icon_widget_defaults = dict(
    font=var.font.default_font_widget,
    background=colors.background_color
)
rect_decoraiton_defaults=dict(
    colour="#2E3440", # "#2b2f37"
    filled=True,
    padding_y=0,
)
decorations_no_round=dict(
    decorations=[
        RectDecoration(
            radius=[ 0, 0, 0, 0 ],
            **rect_decoraiton_defaults
        )
])
decorations_round=dict(
    decorations=[
        RectDecoration(
            radius=[ 17, 17, 17, 17 ],
            **rect_decoraiton_defaults
        )
])
decorations_round_left=dict(
    decorations=[
        RectDecoration(
            radius=[ 17, 0, 0, 17 ],
            **rect_decoraiton_defaults
        )
])
decorations_round_right=dict(
    decorations=[
        RectDecoration(
            radius=[ 0, 17, 17, 0 ],
            **rect_decoraiton_defaults
        )
])
applications_launcher=dict(
    mouse_callbacks = dict(
        Button1=lambda: qtile.spawn(var.run.mymenu),
    ),
)
