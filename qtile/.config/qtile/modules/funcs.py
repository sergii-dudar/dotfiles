import colors
import colors_dt
from libqtile import bar, group, hook, layout, widget
from libqtile.config import Click, Drag, DropDown, Group, Key, Match, Rule, ScratchPad, Screen
from libqtile.lazy import lazy
from libqtile.log_utils import logger
from libqtile.utils import send_notification

previous_focused = []
@hook.subscribe.client_focus
def client_focused(window):
    global previous_focused
    if len(previous_focused) < 2:
        previous_focused.append(window)
    elif previous_focused[1] != window:
        previous_focused[0] = previous_focused[1]
        previous_focused[1] = window
    # logger.info(f"FOCUSED {window}, {previous_focused}")

@lazy.function
def focus_previous_window(qtile):
    global previous_focused
    if len(previous_focused) == 2:
        group = previous_focused[0].group
        qtile.current_screen.set_group(group)
        # logger.info(f"FOCUS PREVIOUS {previous_focused[0]}")
        group.focus(previous_focused[0])

# A function for hide/show all the windows in a group
@lazy.function
def minimize_all(qtile):
    for win in qtile.current_group.windows:
        if hasattr(win, "toggle_minimize"):
            win.toggle_minimize()

# A function for toggling between MAX and MONADTALL layouts
@lazy.function
def maximize_by_switching_layout(qtile):
    current_layout_name = qtile.current_group.layout.name
    if current_layout_name == 'monadtall':
        qtile.current_group.layout = 'max'
    elif current_layout_name == 'max':
        qtile.current_group.layout = 'monadtall'

@hook.subscribe.layout_change
def layout_change(layout, group):
	for window in group.windows:
		window.floating = False
