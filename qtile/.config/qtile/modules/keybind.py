from libqtile import qtile
from libqtile.config import (
    Click,
    Drag,
    DropDown,
    Group,
    Key,
    KeyChord,
    Match,
    Rule,
    ScratchPad,
    Screen,
)
from libqtile.lazy import lazy
from libqtile.log_utils import logger
from modules import funcs
from util import vars

mod = vars.key.mod
alt = vars.key.alt

layout_cycle = ["columns", "max"]
def cycle_layouts(qtile):
    """Switch to the next layout in the custom cycle list."""
    group = qtile.current_group
    current_layout = group.layout.name

    # Find the next layout in the cycle
    next_index = (layout_cycle.index(current_layout) + 1) % len(layout_cycle)
    next_layout = layout_cycle[next_index]

    group.setlayout(next_layout)

keys = [
    Key([mod], "Return", lazy.spawn(vars.run.terminal), desc="Launch terminal"),
    # Toggle between different layouts as defined below
    # Key([mod], "Tab", lazy.next_layout(), desc="Toggle between layouts"),
    Key([mod], "Tab", lazy.function(cycle_layouts), desc="Rotate between declared cycle layouts"),
    Key([mod, "shift"], "c", lazy.window.kill(), desc="Kill focused window"),

    Key([mod], "f", lazy.window.toggle_fullscreen(), desc='toggle fullscreen'),
    Key([mod, "shift"], "m", funcs.minimize_all(), desc="Toggle hide/show all windows on current group"),
    #Key([mod], "m", lazy.layout.maximize(), desc='Toggle between min and max sizes'),
    Key([mod], "m", lazy.function(cycle_layouts), desc='Toggle between min and max sizes'),

    Key([mod], "b", lazy.hide_show_bar(position='all'), desc="Toggles the bar to show/hide"),

    #Key([mod], "w", lazy.window.toggle_floating(), desc="Toggle floating on the focused window"),

    # Toggle between split and unsplit sides of stack.
    # Split = all windows displayed
    # Unsplit = 1 window displayed, like Max layout, but still with
    # multiple stack panes

    Key([mod, "shift"], "r", lazy.reload_config(), desc="Reload the config"),
    Key([mod, "shift"], "x", lazy.shutdown(), desc="Shutdown Qtile"),

    # Movement Keys
    Key([mod], "h", lazy.layout.left(), desc="Move focus to left"),
    Key([mod], "l", lazy.layout.right(), desc="Move focus to right"),
    Key([mod], "j", lazy.layout.down(), desc="Move focus down"),
    Key([mod], "k", lazy.layout.up(), desc="Move focus up"),

    # Key([mod], "space", lazy.layout.next(), desc="Move window focus to other window"),
    Key([mod, "shift"], "h", lazy.layout.shuffle_left(), desc="Move window to the left"),
    Key([mod, "shift"], "l", lazy.layout.shuffle_right(), desc="Move window to the right"),
    Key([mod, "shift"], "j", lazy.layout.shuffle_down(), desc="Move window down"),
    Key([mod, "shift"], "k", lazy.layout.shuffle_up(), desc="Move window up"),

    # Grow windows up, down, left, right.  Only works in certain layouts.
    # Works in 'bsp' and 'columns' layout.
    Key([mod, "control"], "h", lazy.layout.grow_left(), desc="Grow window to the left"),
    Key([mod, "control"], "l", lazy.layout.grow_right(), desc="Grow window to the right"),
    Key([mod, "control"], "j", lazy.layout.grow_down(), desc="Grow window down"),
    Key([mod, "control"], "k", lazy.layout.grow_up(), desc="Grow window up"),
    Key([mod], "n", lazy.layout.normalize(), desc="Reset all window sizes"),

    # Switch focus to specific monitor (out of three)
    Key([mod], "i", lazy.to_screen(0)),
    Key([mod], "o", lazy.to_screen(1)),
    # Switch focus of monitors
    Key([mod], "period", lazy.next_screen()),
    Key([mod], "comma", lazy.prev_screen()),
    # Key([mod], "Tab", lazy.group.focus_back(), desc="Alternate between two most recent windows")
    #Key([mod], "Tab", lazy.function(focus_previous_window)),
    Key([alt], "Tab", funcs.focus_previous_window()),

    # Key([mod], "space", lazy.widget["keyboardlayout"].next_keyboard(), desc="Next keyboard layout."),

     # Left, Up, Right, Down
    # Key([alt], "Right", lazy.screen.next_group(), desc="Move to next group."),
    # Key([alt], "Left", lazy.screen.prev_group(), desc="Move to previous group."),
    #Key([mod], "Right", lazy.screen.next_group(), desc="Move to next group."),
    #Key([mod], "Left", lazy.screen.prev_group(), desc="Move to previous group."),

    #Key([mod], "Tab", lazy.screen.next_group(), desc="Move to next group."),
    #Key([mod, "shift"], "Tab", lazy.screen.prev_group(), desc="Move to previous group."),

    Key([mod], "Left", lazy.spawn("amixer -D pulse sset Master 5%-"), desc="Decrease volume to -5."),
    Key([mod], "Right", lazy.spawn("amixer -D pulse sset Master 5%+"), desc="Increase volume to +5."),
    Key([mod], "Down", lazy.spawn("amixer -D pulse sset Master toggle"), desc="Toggle volume."),
]

# Drag floating layouts.
mouse = [
    Drag([mod], "Button1", lazy.window.set_position_floating(), start=lazy.window.get_position()),
    Drag([mod], "Button3", lazy.window.set_size_floating(), start=lazy.window.get_size()),
    Click([mod], "Button2", lazy.window.bring_to_front()),
]

# Add key bindings to switch VTs in Wayland.
# We can't check qtile.core.name in default config as it is loaded before qtile is started
# We therefore defer the check until the key binding is run by using .when(func=...)
for vt in range(1, 8):
    keys.append(
        Key(
            ["control", alt],
            f"f{vt}",
            lazy.core.change_vt(vt).when(func=lambda: qtile.core.name == "wayland"),
            desc=f"Switch to VT{vt}",
        )
    )
