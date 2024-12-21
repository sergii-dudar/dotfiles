import os


class PathClass:
    def __init__(self):
        self.home_dir = os.path.expanduser("~")
path = PathClass()

class KeyClass:
    def __init__(self):
        self.mod = "mod4"
        self.alt="mod1"
key = KeyClass()

class FontClass:
    def __init__(self):
        self.default_font = "CaskaydiaCove Nerd Font Bold"
        self.default_font_size = 17.5
        self.default_font_widget = "CaskaydiaCove Nerd Font Bold"
        self.default_font_widget_size = 23
font = FontClass()

class RunClass:
    def __init__(self):
        self.terminal_kitty = "kitty"
        self.terminal = "wezterm"
        #mymenu = "rofi -show drun"
        self.mymenu = path.home_dir + "/.config/rofi/scripts/launcher_t1"
        self.powermenu = path.home_dir + "/.config/rofi/scripts/powermenu_t1"
        self.browser = "google-chrome-stable"
        self.files = "nautilus"
        self.discord = "webcord"
        self.todoist = "flatpak run com.todoist.Todoist"
        self.screenie = "flameshot gui"
run = RunClass()

class VarsClass:
    def __init__(self):
        self.path = path
        self.key = key
        self.font = font
        self.run = run
var = VarsClass()
