from libqtile.config import DropDown, Key, KeyChord, Match, ScratchPad
from libqtile.lazy import lazy
from libqtile.log_utils import logger
from libqtile.utils import send_notification
from modules.variables import mod


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

def add_scratchpad(groups, keys):
    # Define scratchpads
    groups.append(
        ScratchPad(
            "scratchpad",
            [
                # DropDown("term", "kitty --class=scratch", width=0.8, height=0.8, x=0.1, y=0.1, opacity=1),
                # DropDown("term2", "kitty --class=scratch", width=0.8, height=0.8, x=0.1, y=0.1, opacity=1),
                # DropDown("ranger", "kitty --class=ranger -e ranger", width=0.8 h,eight=0.8, x=0.1, y=0.1, opacity=0.9),
                # DropDown("volume", "kitty --class=volume -e pulsemixer", width=0.8, height=0.8, x=0.1, y=0.1, opacity=0.9),
                # DropDown("mus", "kitty --class=mus -e flatpak run io.github.hrkfdn.ncspot", width=0.8, height=0.8, x=0.1, y=0.1, opacity=0.9),
                # DropDown("news", "kitty --class=news -e newsboat", width=0.8, height=0.8, x=0.1, y=0.1, opacity=0.9),

                DropDown("telegram",
                         "telegram-desktop",
                         width=telegram_width,
                         height=telegram_height,
                         x=telegram_x,
                         y=telegram_y,
                         opacity=0.98),
                DropDown("yazi",
                         "kitty --class=yazi -e yazi",
                         width=yazi_width,
                         height=yazi_height,
                         x=yazi_x,
                         y=yazi_y,
                         opacity=0.9),

                # install from chrome, and check ~/.local/share/applications/*.desktop right launch command
                DropDown("youtube_music",
                         "google-chrome-stable --profile-directory=Default --app-id=cinhimbnkkaeohfgghhklpknlkffjgod",
                         width=yazi_width,
                         height=yazi_height,
                         x=yazi_x,
                         y=yazi_y,
                         opacity=0.9,
                         match=Match(wm_class="Google-chrome", wm_instance_class="crx_cinhimbnkkaeohfgghhklpknlkffjgod")),
                DropDown("google_chat",
                         "google-chrome-stable --profile-directory=Default --app-id=mdpkiolbdkhdjpekfbkbmhigcaggjagi",
                         width=yazi_width,
                         height=yazi_height,
                         x=yazi_x,
                         y=yazi_y,
                         opacity=0.9,
                         match=Match(wm_class="Google-chrome", wm_instance_class="crx_mdpkiolbdkhdjpekfbkbmhigcaggjagi")),
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
            Key([mod], "m", lazy.group["scratchpad"].dropdown_toggle("youtube_music")),
            Key([mod], "g", lazy.group["scratchpad"].dropdown_toggle("google_chat")),

            KeyChord([mod], "p", [
                Key([], "y", lazy.group["scratchpad"].dropdown_toggle("yazi")),
                Key([], "t", lazy.group["scratchpad"].dropdown_toggle("telegram")),
                Key([], "m", lazy.group["scratchpad"].dropdown_toggle("youtube_music")),
                Key([], "g", lazy.group["scratchpad"].dropdown_toggle("google_chat")),
            ], name="Scratchpad")
        ]
    )
