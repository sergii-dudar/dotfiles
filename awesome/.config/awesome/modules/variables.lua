local M = {}
M.path = {}
M.app = {}
M.key = {}

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

-- {{{ Variable definitions
-- Themes define colours, icons, font and wallpapers.
-- beautiful.init(gears.filesystem.get_themes_dir() .. "default/theme.lua")
--beautiful.init(variables.custom_theme)
local all_themes = {
    "personal", --1
    "blackburn", -- 2
    "copland", -- 3
    "dremora", -- 4
    "holo", -- 5
    "multicolor", -- 6
    "powerarrow", -- 7
    "powerarrow-dark", -- 8
    "rainbow", -- 9
    "steamburn", -- 10
    "vertex", -- 11
}
local choosen_theme_name = all_themes[1]
M.current_theme_dir = string.format("%s/.config/awesome/themes/%s", M.path.home_dir, choosen_theme_name)
M.current_theme_path = string.format("%s/theme.lua", M.current_theme_dir)

return M
