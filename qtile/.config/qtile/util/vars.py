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

        self.change_language = "bash " + path.home_dir + "/dotfiles/bin/change_language.sh"
        self.monkey_type = "google-chrome-stable --profile-directory=Default --app-id=picebhhlijnlefeleilfbanaghjlkkna"
        self.volume_control = "pavucontrol"
        self.gnome_system_monitor = "gnome-system-monitor"
        self.gnome_settings = "env XDG_CURRENT_DESKTOP=GNOME gnome-control-center"
        self.gnome_clocks = "gnome-clocks"
        self.gnome_calendar = "gnome-calendar"
        self.htop = "kitty --name htop_info -e htop"
        self.disc_usage = 'kitty --name disc_usage_info --hold zsh -c "export MANUAL_RL=1; df; exec zsh"'
        self.disc_gdu = "kitty --name disc_ugd -e gdu"

        self.editor = os.getenv("EDITOR") or "neovim"
        self.editor_cmd = self.terminal + " -e " + self.editor
run = RunClass()

class SettingsClass:
    def __init__(self):
        self.screen_width=3840
        self.screen_height=2160
        self.default_factor_width = 0.65
        self.default_factor_height = 0.7
settings = SettingsClass()

class VarsClass:
    def __init__(self):
        self.path = path
        self.key = key
        self.font = font
        self.run = run
        self.settings = settings
var = VarsClass()
