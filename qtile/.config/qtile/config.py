import os
import subprocess

# Make sure 'qtile-extras' is installed or this config will not work.
from qtile_extras import widget
from qtile_extras.widget.decorations import BorderDecoration

from libqtile import bar, group, hook, layout, qtile, widget
from libqtile.config import Click, Drag, DropDown, Group, Key, Match, Rule, ScratchPad, Screen
from libqtile.lazy import lazy
from libqtile.log_utils import logger
from modules import (
    funcs,
    keybind,
    scratchpad,
    widgets,
    winrules,
    workspaces,
)

keys = keybind.keys
mouse = keybind.mouse
groups = workspaces.groups
workspaces.extend_keys_with_groups(keys)

layouts = workspaces.layouts
scratchpad.add_scratchpad(groups, keys)

# outside gaps: screen[right, left, bottom]
# inside gaps: layout.margin
screens = [
    Screen(
        right=bar.Gap(3),
        left=bar.Gap(3),
        bottom=bar.Gap(3),
        top=bar.Bar(
            widgets.build_main_bar_widgets(),
            size=35,
            # margin=[ 0, 0, 7, 0 ],
            margin=[ 0, 0, 0, 0 ],
        ),
    ),
    Screen(
        right=bar.Gap(3),
        left=bar.Gap(3),
        bottom=bar.Gap(3),
        top=bar.Bar(
            widgets.build_second_bar_widgets(),
            size=35,
            # margin=[ 0, 0, 7, 0 ],
            margin=[ 0, 0, 0, 0 ],
        ),
        # layouts = [layout.verticaltile()]
        # layout = layout.verticaltile()
    ),
]

dgroups_key_binder = None
dgroups_app_rules = []  # type: list
follow_mouse_focus = True
bring_front_click = False
floats_kept_above = True
cursor_warp = False

floating_layout = winrules.floating_layout

auto_fullscreen = True
focus_on_window_activation = "smart"
reconfigure_screens = True


# If things like steam games want to auto-minimize themselves when losing
# focus, should we respect this or not?
auto_minimize = True

# When using the Wayland backend, this can be used to configure input devices.
wl_input_rules = None

wmname = "qtile"
