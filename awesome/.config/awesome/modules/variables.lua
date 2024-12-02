local M = {}

M.home_dir = os.getenv("HOME")
M.mymenu = M.home_dir .. "/.config/rofi/scripts/launcher_t1"
M.powermenu = M.home_dir .. "/.config/rofi/scripts/powermenu_t1"
M.custom_theme = M.home_dir .. "/.config/awesome/themes/personal/theme.lua"

return M