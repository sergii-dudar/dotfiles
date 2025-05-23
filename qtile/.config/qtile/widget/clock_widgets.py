from qtile_extras import widget

from libqtile import qtile
from util import colors
from util.util import to_span

colors = colors.current()

class MouseClickClock(widget.Clock):
    # defaults = [
    #     (
    #         "long_format",
    #         "%A %d %B %Y | %H:%M",
    #         "Format to show when mouse is over widget."
    #     ),
    #     (
    #         "date",
    #         "%a, %b %d",
    #         "Format to show on click."
    #     )
    # ]
    #
    # def __init__(self, **config):
    #     widget.Clock.__init__(self, **config)
    #     self.add_defaults(MouseOverClock.defaults)
    #     self.short_format = self.format
    #
    # def mouse_enter(self, *args, **kwargs):
    #     self.format = self.long_format
    #     self.bar.draw()
    #
    # def mouse_leave(self, *args, **kwargs):
    #     self.format = self.short_format
    #     self.bar.draw()

    def __init__(self, **config):
        widget.Clock.__init__(self, **config)
        self.formats = [
            # to_span("  ", colors.colors.color6[0], 17) +
            # to_span(" ", colors.colors.color6[0], 5) +
            # to_span("%I:%M", colors.colors.color6[0]) +
            # to_span(" ", colors.colors.color6[0], 5) +
            # to_span("%p", colors.colors.color6[0])
            #             , "%A, %B %d"
        ]
        self.current_format_index = 0
        self.format = self.formats[self.current_format_index]
        self.add_callbacks(dict(
            Button1=self.change_format,
            Button3=lambda: qtile.spawn("gnome-calendar"),
        ))

    def change_format(self):
        self.current_format_index = (self.current_format_index + 1) % len(self.formats)
        self.format = self.formats[self.current_format_index]
        self.bar.draw()
