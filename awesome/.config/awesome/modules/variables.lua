local M = {}
M.path = {}
M.app = {}
M.key = {}
M.font = {}

M.path.home_dir = os.getenv("HOME")
M.path.mymenu = M.path.home_dir .. "/.config/rofi/scripts/launcher_t1"
M.path.powermenu = M.path.home_dir .. "/.config/rofi/scripts/powermenu_t1"

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
M.font.widget = M.font.to_size(M.font.default_size)

local theme_name = "personal"
M.current_theme_dir = string.format("%s/.config/awesome/themes/%s", M.path.home_dir, theme_name)
M.current_theme_path = string.format("%s/theme.lua", M.current_theme_dir)
M.current_colors_path = string.format("%s/colors.lua", M.current_theme_dir)

return M
