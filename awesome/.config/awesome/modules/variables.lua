local awful = require("awful")

local M = {}
M.path = {}
M.app = {}
M.key = {}
M.font = {}
M.run = {}
M.settings = {}
M.widget = {}

M.path.home_dir = os.getenv("HOME")

M.run.mymenu = M.path.home_dir .. "/.config/rofi/scripts/launcher_t1"
M.run.powermenu = M.path.home_dir .. "/.config/rofi/scripts/powermenu_t1"
M.run.change_language = "bash " .. M.path.home_dir .. "/dotfiles/bin/change_language.sh"
M.run.monkey_type = "google-chrome-stable --profile-directory=Default --app-id=picebhhlijnlefeleilfbanaghjlkkna"
M.run.volume_control = "pavucontrol"
M.run.gnome_system_monitor = "gnome-system-monitor"
M.run.gnome_settings = "XDG_CURRENT_DESKTOP=GNOME gnome-control-center"
M.run.gnome_clocks = "gnome-clocks"
M.run.gnome_calendar = "gnome-calendar"
M.run.htop = "kitty --name htop_info -e htop"
M.run.disc_usage = 'kitty --name disc_usage_info --hold zsh -c "export MANUAL_RL=1; df; exec zsh"'
-- This is used later as the default terminal and editor to run.
M.app.terminal = "wezterm"
M.app.editor = os.getenv("EDITOR") or "neovim"
M.app.editor_cmd = M.app.terminal .. " -e " .. M.app.editor

-- Default modkey.
-- Usually, Mod4 is the key with a logo between Control and Alt.
-- If you do not like this or do not have such a key,
-- I suggest you to remap Mod4 to another key using xmodmap or other tools.
-- However, you can use another modifier like Mod1, but it may interact with others.
M.key.modkey = "Mod4"
M.key.altkey = "Mod1"

M.font.to_size = function(zise)
    return "CaskaydiaCove Nerd Font Bold " .. zise
end
M.font.default_size = 14
M.font.default = M.font.to_size(M.font.default_size)
M.font.widget = M.font.default

-- Get the focused screen's geometry
local screen_geometry = awful.screen.focused().geometry
M.settings.screen_width = screen_geometry.width
M.settings.screen_height = screen_geometry.height
M.settings.default_factor_width = 0.65
M.settings.default_factor_height = 0.7

local bg_color = "#2E3440" --"#2D2A2E" --"#2B3339"
M.widget.bg_color = bg_color
M.widget.bg_color_hover = "#1e1e2e"
M.widget.icon_widget_space = 5

local theme_name = "personal"
M.current_theme_dir = string.format("%s/.config/awesome/themes/%s", M.path.home_dir, theme_name)
M.current_theme_path = string.format("%s/theme.lua", M.current_theme_dir)
M.current_colors_path = string.format("%s/colors.lua", M.current_theme_dir)

return M
