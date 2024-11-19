import os
import subprocess

import colors
from libqtile import bar, group, hook, layout, qtile, widget
from libqtile.config import Click, Drag, DropDown, Group, Key, Match, Rule, ScratchPad, Screen
from libqtile.lazy import lazy
from libqtile.log_utils import logger
from libqtile.utils import send_notification

mod = "mod4"
alt="mod1"
terminal_kitty = "kitty"
terminal = "wezterm"
mymenu = "rofi -show drun"
browser = "google-chrome-stable"
files = "nautilus"
discord = "webcord"
todoist = "flatpak run com.todoist.Todoist"
screenie = "flameshot gui"

default_font = "CaskaydiaCove Nerd Font"
default_font_size = 16
default_font_widget = "CaskaydiaCove Nerd Font Bold"
default_font_widget_size = 18

colors, backgroundColor, foregroundColor, workspaceColor, chordColor = colors.dwm()

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

keys = [
    Key([mod], "Return", lazy.spawn(terminal), desc="Launch terminal"),
    # Toggle between different layouts as defined below
    #Key([mod], "Tab", lazy.next_layout(), desc="Toggle between layouts"),
    Key([mod, "shift"], "c", lazy.window.kill(), desc="Kill focused window"),
    Key([mod], "f", lazy.window.toggle_fullscreen(), desc="Toggle fullscreen on the focused window"),
    # Key([mod], "f", lazy.window.toggle_floating(), desc="Toggle floating on the focused window"),
    Key([mod, "shift"], "r", lazy.reload_config(), desc="Reload the config"),
    Key([mod, "shift"], "x", lazy.shutdown(), desc="Shutdown Qtile"),
    Key([mod], "r", lazy.spawncmd(), desc="Spawn a command using a prompt widget"),
    Key([alt], "space", lazy.spawn(mymenu)),
    Key([mod], "w", lazy.spawn(browser)),
    Key([mod, "shift"], "Return", lazy.spawn(files)),
    Key([mod, alt], "s", lazy.spawn(screenie)),
    Key([alt], "s", lazy.spawn(todoist)),
    Key([alt], "n", lazy.spawn(discord)),
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
    Key([alt], "Tab", focus_previous_window()),

    # Key([mod], "space", lazy.widget["keyboardlayout"].next_keyboard(), desc="Next keyboard layout."),

     # Left, Up, Right, Down
    # Key([alt], "Right", lazy.screen.next_group(), desc="Move to next group."),
    # Key([alt], "Left", lazy.screen.prev_group(), desc="Move to previous group."),
    Key([mod], "Right", lazy.screen.next_group(), desc="Move to next group."),
    Key([mod], "Left", lazy.screen.prev_group(), desc="Move to previous group."),

    Key([mod], "Tab", lazy.screen.next_group(), desc="Move to next group."),
    Key([mod, "shift"], "Tab", lazy.screen.prev_group(), desc="Move to previous group."),
]

# Create labels for groups and assign them a default layout.
groups = []

group_names = ["1", "2", "3", "4", "5", "6", "7", "8", "9"]

#group_labels = ["󰖟", "", "", "", "", "", "", "", "ﭮ", "", "", "﨣", "F1", "F2", "F3", "F4", "F5"]
group_labels = group_names

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

# Add group specific keybindings
for i in groups:
    keys.extend(
        [
            Key([mod], i.name, lazy.group[i.name].toscreen(), desc="Mod + number to move to that group."),
            Key([mod, "shift"], i.name, lazy.window.togroup(i.name), desc="Move focused window to new group."),
            # Key([alt], "Tab", lazy.screen.next_group(), desc="Move to next group."),
            # Key([alt, "shift"], "Tab", lazy.screen.prev_group(), desc="Move to previous group."),
        ]
    )

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

############################################################
###### Open specific applications in scratchpad mode #######
############################################################

def to_center_x(width: float) -> float:
    return (1 - width) / 2

def to_center_y(height: float) -> float:
    return (1 - height) / 2

telegram_width=0.55
telegram_height=0.6
telegram_x=to_center_x(width=telegram_width)
telegram_y=to_center_y(height=telegram_height)

yazi_width=0.8
yazi_height=yazi_width
yazi_x=to_center_x(width=yazi_width)
yazi_y=to_center_y(height=yazi_height)

# Define scratchpads
groups.append(
    ScratchPad(
        "scratchpad",
        [
            # DropDown("term", "kitty --class=scratch", width=0.8, height=0.8, x=0.1, y=0.1, opacity=1),
            # DropDown("term2", "kitty --class=scratch", width=0.8, height=0.8, x=0.1, y=0.1, opacity=1),
            # DropDown("ranger", "kitty --class=ranger -e ranger", width=0.8, height=0.8, x=0.1, y=0.1, opacity=0.9),
            # DropDown("volume", "kitty --class=volume -e pulsemixer", width=0.8, height=0.8, x=0.1, y=0.1, opacity=0.9),
            # DropDown("mus", "kitty --class=mus -e flatpak run io.github.hrkfdn.ncspot", width=0.8, height=0.8, x=0.1, y=0.1, opacity=0.9),
            # DropDown("news", "kitty --class=news -e newsboat", width=0.8, height=0.8, x=0.1, y=0.1, opacity=0.9),

            DropDown("telegram", "telegram-desktop &", width=telegram_width, height=telegram_height, x=telegram_x, y=telegram_y, opacity=0.98),
            DropDown("yazi", "kitty --class=yazi -e yazi", width=yazi_width, height=yazi_height, x=yazi_x, y=yazi_y, opacity=0.9),
        ],
    )
)

# Scratchpad keybindings
keys.extend(
    [
        # Key([mod], "n", lazy.group["scratchpad"].dropdown_toggle("term")),
        #Key([mod], "c", lazy.group["scratchpad"].dropdown_toggle("ranger")),
        # Key([mod], "v", lazy.group["scratchpad"].dropdown_toggle("volume")),
        # Key([mod], "m", lazy.group["scratchpad"].dropdown_toggle("mus")),
        # Key([mod], "b", lazy.group["scratchpad"].dropdown_toggle("news")),
        # Key([mod, "shift"], "n", lazy.group["scratchpad"].dropdown_toggle("term2")),

        Key([mod], "y", lazy.group["scratchpad"].dropdown_toggle("yazi")),
        Key([mod], "t", lazy.group["scratchpad"].dropdown_toggle("telegram")),
    ]
)


# Define layouts and layout themes
layout_theme = {
    "margin": 4,
    "border_width": 3,
    "border_focus": colors[5],
    "border_normal": colors[0],
    "border_on_single": True
}

layouts = [
    layout.Columns(**layout_theme),
    layout.Max(**layout_theme)
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
    # layout.Stack(num_stacks=2),
    # layout.Bsp(),
    # layout.Matrix(),
    # layout.MonadTall(),
    # layout.MonadWide(),
    # layout.RatioTile(),
    # layout.Tile(),
    # layout.TreeTab(),
    # layout.VerticalTile(),
    # layout.Zoomy(),
]

sep = widget.Sep(linewidth=1, paddog=15, foreground=colors[0], background=colors[0])
spacer = widget.Spacer(background=colors[11])
groupbox = widget.GroupBox(
    font="CaskaydiaCove Nerd Font",
    fontsize=20,
    margin_y=4,
    margin_x=4,
    padding_y=6,
    padding_x=6,
    borderwidth=2,
    disable_drag=True,
    active=colors[2], # unfocused
    inactive=colors[3],
    hide_unused=False,
    rounded=False,
    highlight_method="text",
    highlight_color=colors[0], # box color
    this_current_screen_border=colors[6],
    this_screen_border=colors[10],
    other_current_screen_border=colors[2],
    block_highlight_text_color=colors[6],
    other_screen_border=colors[6],
    urgent_alert_method="line",
    urgent_border=colors[6],
    urgent_text=colors[1],
    foreground=colors[0],
    background=colors[0],
    use_mouse_wheel=False)

# weather = widget.OpenWeather(
#     app_key="4cf3731a25d1d1f4e4a00207afd451a2",
#     cityid="4997193",
#     format="{icon} {main_temp}°",
#     metric=False,
#     font=default_font,
#     fontsize=13,
#     background=colors[0],
#     foreground=colors[2],
# )

volicon = widget.TextBox(text="󰕾", fontsize=25, font=default_font, foreground=colors[2], background=colors[0])
volume = widget.Volume(foreground=colors[2], padding=10, background=colors[0])
cpuicon = widget.TextBox(text="", fontsize=20, font=default_font, background=colors[0], foreground=colors[3])
cpu = widget.CPU(font=default_font_widget, update_interval=1.0, format="{load_percent}%", foreground=colors[2], background=colors[0], padding=5)
memicon = widget.TextBox(text="", fontsize=20, font=default_font, background=colors[0], foreground=colors[6])
mem = widget.Memory(
    font=default_font_widget,
    foreground=colors[2],
    background=colors[0],
    format="{MemUsed: .0f}{mm} /{MemTotal: .0f}{mm}",
    measure_mem="G",
    padding=5,
)
clockicon = widget.TextBox(text="", fontsize=20, font=default_font, background=colors[0], foreground=colors[5])
clock = widget.Clock(format="%I:%M %p", font=default_font_widget, padding=10, background=colors[0], foreground=colors[2])
curlayout = widget.CurrentLayoutIcon(
    scale=0.5,
    foreground=colors[0],
    background=colors[11],
    padding=10,
)
tray = widget.Systray(background=colors[0])
windowname = widget.WindowName(
    font=default_font_widget,
    foreground=colors[0],
    background=colors[6]
)
keyboard = widget.KeyboardLayout(
    configured_keyboards=['us','ua'],
    font = default_font_widget,
    fontsize = 18,
    fmt = "  {}"
)
screens = [
    Screen(
        right=bar.Gap(7),
        left=bar.Gap(7),
        bottom=bar.Gap(7),
        top=bar.Bar(
            [
                # left
                sep,
                curlayout,
                windowname,
                spacer,

                # center
                groupbox,
                clockicon,
                clock,
                spacer,

                # right
                #keyboard,
                keyboard,
                sep,
                volicon,
                volume,
                cpuicon,
                cpu,
                memicon,
                mem,
                sep,
                sep,
                sep,
                tray,
                sep,
                sep,
            ],
            size=35,
            margin=[ 0, 0, 7, 0 ],
        ),
    ),
]

# Drag floating layouts.
mouse = [
    Drag([mod], "Button1", lazy.window.set_position_floating(), start=lazy.window.get_position()),
    Drag([mod], "Button3", lazy.window.set_size_floating(), start=lazy.window.get_size()),
    Click([mod], "Button2", lazy.window.bring_to_front()),
]

dgroups_key_binder = None
dgroups_app_rules = []  # type: list
follow_mouse_focus = True
bring_front_click = False
floats_kept_above = True
cursor_warp = False

############################################################
####### Open specific applications in floating mode ########
############################################################

floating_layout = layout.Floating(
    float_rules=[
        # Run the utility of `xprop` to see the wm class and name of an X client.
        *layout.Floating.default_float_rules,
        Match(wm_class="confirmreset"),  # gitk
        Match(wm_class="makebranch"),  # gitk
        Match(wm_class="maketag"),  # gitk
        Match(wm_class="ssh-askpass"),  # ssh-askpass
        Match(title="branchdialog"),  # gitk
        Match(title="pinentry"),  # GPG key password entry

        Match(wm_class="qBittorrent"),
        Match(wm_class="pavucontrol"),
        Match(wm_class="org.gnome.Nautilus"),
        Match(wm_class="gnome-system-monitor"),
        Match(wm_class="Nm-connection-editor"),
        Match(wm_class="ViberPC"),
        Match(wm_class="vlc"),
        Match(wm_class="gnome-calculator"),
        Match(wm_class="snapshot"),
        Match(wm_class="Gcolor3"),
        Match(wm_class="org.gnome.Characters"),
        Match(wm_class="org.gnome.clocks"),
        Match(wm_class="gnome-calendar"),
        Match(wm_class="Gnome-disks"),
        Match(wm_class="Glate"),

        #Match(title="Telegram"),

        # Google Chat
        #Match(wm_class="Google-chrome", wm_instance_class="crx_mdpkiolbdkhdjpekfbkbmhigcaggjagi"),
        # Monkeytype
        Match(wm_class="Google-chrome", wm_instance_class="crx_picebhhlijnlefeleilfbanaghjlkkna")
    ]
)

############################################################
######## Open applications on specific workspaces ##########
############################################################

# Define rules to assign specific applications to groups
rules_list = [
        { "rule": Rule(Match(wm_class="org.wezfurlong.wezterm")), "group": "1" },
        { "rule": Rule(Match(wm_class="jetbrains-idea")), "group": "2" },
        { "rule": Rule(Match(wm_class="Code")), "group": "2" },
        { "rule": Rule(Match(wm_class="Google-chrome", wm_instance_class="google-chrome")), "group": "3" },
        { "rule": Rule(Match(wm_class="kitty")), "group": "4" },
]

@hook.subscribe.client_new
def assign_app_group(client):
    for item in rules_list:
        if item["rule"].matches(client):
            client.togroup(item["group"])
            break

auto_fullscreen = True
focus_on_window_activation = "smart"
reconfigure_screens = True

# @hook.subscribe.screen_change
# def screen_change():
#     # Force Qtile to reconfigure screens and groups
#      lazy.reload_config()

# @hook.subscribe.startup_complete
# def run_every_startup():
#     lazy.reload_config()
#     send_notification("qtile", "startup_complete")

@hook.subscribe.screen_change
def screen_change():
    lazy.reload_config()
    send_notification("qtile", "screen_change")

@hook.subscribe.startup_once
def autostart_once():
    home = os.path.expanduser("~/.config/qtile/shell/autostart_once.sh")
    subprocess.Popen([home])

@hook.subscribe.startup
def autostart_always():
    home = os.path.expanduser("~/.config/qtile/shell/autostart_always.sh")
    subprocess.Popen([home])

# If things like steam games want to auto-minimize themselves when losing
# focus, should we respect this or not?
auto_minimize = True

# When using the Wayland backend, this can be used to configure input devices.
wl_input_rules = None

wmname = "qtile"
