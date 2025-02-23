import os
import subprocess

from libqtile import bar, group, hook, layout, qtile, widget
from libqtile.config import Click, Drag, DropDown, Group, Key, Match, Rule, ScratchPad, Screen
from libqtile.lazy import lazy
from util import colors
from util.vars import key

alt, mod = key.alt, key.mod

colors = colors.current()

# Create labels for groups and assign them a default layout.
groups = []

group_names = ["1", "2", "3", "4", "5", "6", "7", "8", "9"]

#group_labels = ["󰖟", "", "", "", "", "", "", "", "ﭮ", "", "", "﨣", "F1", "F2", "F3", "F4", "F5"]
#group_labels = ["DEV", "WWW", "SYS", "DOC", "VBOX", "CHAT", "MUS", "VID", "GFX",]
group_labels = ["1 ", "2 ", "3 ", "4 ", "5 ", "6 󰣇", "7 ", "8 ", "9 ",]
#group_labels = ["", "", "", "", "", "", "", "", "",]
#group_labels = group_names

group_layout = "columns"

# Add group names, labels, and default layouts to the groups object.
for i in range(len(group_names)):
    groups.append(
        Group(
            name=group_names[i],
            layout=group_layout,
            label=group_labels[i],
        )
    )

#groups = [Group(i) for i in "123456789"]

def extend_keys_with_groups(keys):
    # Add group specific variables.ngs
    for i in groups:
        keys.extend(
            [
                Key([mod], i.name, lazy.group[i.name].toscreen(), desc="Mod + number to move to that group."),
                Key([mod, "shift"], i.name, lazy.window.togroup(i.name), desc="Move focused window to new group."),
                # Key([alt], "Tab", lazy.screen.next_group(), desc="Move to next group."),
                # Key([alt, "shift"], "Tab", lazy.screen.prev_group(), desc="Move to previous group."),
            ]
        )

# Define layouts and layout themes
layout_theme = {
    "margin": 4,
    "border_width": 4,
    # "border_focus": colors[5],
    # "border_normal": colors[0],
    "border_focus": colors.border_focus,
    "border_normal": colors.border_normal,
    "border_on_single": True
}

layouts = [
    layout.Columns(**layout_theme),
    layout.Max(**layout_theme),
    # layout.TreeTab(**layout_theme),
    # layout.Zoomy(**layout_theme),

    # layout.Columns(border_focus_stack=["#d75f5f", "#8f3d3d"], border_width=4),
    # layout.Max(),
    #
    # layout.MonadTall(**layout_theme),
    # layout.MonadWide(**layout_theme),
    # layout.MonadThreeCol(**layout_theme),
    # layout.MonadWide(**layout_theme),
    # layout.Floating(**layout_theme),
    # layout.Spiral(**layout_theme),
    # layout.RatioTile(**layout_theme),

    # Try more layouts by unleashing below layouts.
    # layout.Stack(num_stacks=3),
    # layout.Bsp(),
    # layout.Matrix(),
    # layout.MonadTall(),
    # layout.MonadWide(),
    # layout.RatioTile(),
    # layout.Tile(),
    # layout.VerticalTile(),
]