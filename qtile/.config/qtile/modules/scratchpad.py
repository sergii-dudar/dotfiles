from libqtile.config import DropDown, Key, KeyChord, Match, ScratchPad
from libqtile.lazy import lazy
from libqtile.log_utils import logger
from libqtile.utils import send_notification
from util.vars import key

mod = key.mod

def to_center_x(width: float) -> float:
    return (1 - width) / 2

def to_center_y(height: float) -> float:
    return (1 - height) / 2

# telegram_width=0.55
# telegram_height=0.6
# telegram_x=to_center_x(width=telegram_width)
# telegram_y=to_center_y(height=telegram_height)

default_rate_width=0.75
default_rate_height=0.8
default_x=to_center_x(width=default_rate_width)
default_y=to_center_y(height=default_rate_height)

def add_scratchpad(groups, keys):
    # Define scratchpads
    groups.append(
        ScratchPad(
            "scratchpad", [
                DropDown("telegram",
                         "Telegram",
                         width=default_rate_width,
                         height=default_rate_height,
                         x=default_x,
                         y=default_y,
                         # opacity=0.98 -- moved to picom
                         ),
                DropDown("yazi",
                         # "kitty --class=yazi -e yazi",
                         "ghostty --class=com.scratchpad.yazi -e yazi",
                         width=default_rate_width,
                         height=default_rate_height,
                         x=default_x,
                         y=default_y,
                         # opacity=0.9 -- moved to picom
                         ),
                DropDown("nautilus",
                         "nautilus",
                         width=default_rate_width,
                         height=default_rate_height,
                         x=default_x,
                         y=default_y
                         ),
                # install from chrome, and check ~/.local/share/applications/*.desktop right launch command
                DropDown("youtube_music",
                         "brave --profile-directory=Default --app-id=cinhimbnkkaeohfgghhklpknlkffjgod",
                         width=default_rate_width,
                         height=default_rate_height,
                         x=default_x,
                         y=default_y,
                         # opacity=0.9, -- moved to picom
                         match=Match(wm_class="Brave-browser", wm_instance_class="crx_cinhimbnkkaeohfgghhklpknlkffjgod")),
                DropDown("google_chat",
                         "brave --profile-directory=Default --app-id=mdpkiolbdkhdjpekfbkbmhigcaggjagi",
                         width=default_rate_width,
                         height=default_rate_height,
                         x=default_x,
                         y=default_y,
                         # opacity=0.9, -- moved to picom
                         match=Match(wm_class="Brave-browser", wm_instance_class="crx_mdpkiolbdkhdjpekfbkbmhigcaggjagi")),
                DropDown("monkeytype",
                         "brave --profile-directory=Default --app-id=picebhhlijnlefeleilfbanaghjlkkna",
                         width=default_rate_width,
                         height=default_rate_height,
                         x=default_x,
                         y=default_y,
                         # opacity=0.95, -- moved to picom
                         match=Match(wm_class="Brave-browser", wm_instance_class="crx_picebhhlijnlefeleilfbanaghjlkkna")),
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

            # Key([mod], "y", lazy.group["scratchpad"].dropdown_toggle("yazi")),
            # Key([mod], "t", lazy.group["scratchpad"].dropdown_toggle("telegram")),
            # Key([mod], "m", lazy.group["scratchpad"].dropdown_toggle("youtube_music")),
            # Key([mod], "g", lazy.group["scratchpad"].dropdown_toggle("google_chat")),
            # Key([mod], "u", lazy.group["scratchpad"].dropdown_toggle("monkeytype")),
            # Key([mod], "n", lazy.group["scratchpad"].dropdown_toggle("nautilus")),

            KeyChord([mod], "p", [
                Key([], "y", lazy.group["scratchpad"].dropdown_toggle("yazi")),
                Key([], "t", lazy.group["scratchpad"].dropdown_toggle("telegram")),
                Key([], "m", lazy.group["scratchpad"].dropdown_toggle("youtube_music")),
                Key([], "g", lazy.group["scratchpad"].dropdown_toggle("google_chat")),
                Key([], "u", lazy.group["scratchpad"].dropdown_toggle("monkeytype")),
                Key([], "n", lazy.group["scratchpad"].dropdown_toggle("nautilus")),
            ], name="Scratchpad")
        ]
    )