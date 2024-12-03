local M = {}
M.path = {}
M.app = {}
M.key = {}

M.path.home_dir = os.getenv("HOME")
M.path.mymenu = M.path.home_dir .. "/.config/rofi/scripts/launcher_t1"
M.path.powermenu = M.path.home_dir .. "/.config/rofi/scripts/powermenu_t1"
M.path.custom_theme = M.path.home_dir .. "/.config/awesome/themes/personal/theme.lua"

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

return M
