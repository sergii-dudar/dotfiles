local M = {}

-- Set programs that you use
-- local terminal = "ghostty"
-- local terminal = "alacritty"
M.terminal = "foot"
M.fileManager = "nautilus"
M.menu = "~/.config/rofi/scripts/launcher_t1"
M.qmenu = "~/.config/rofi/scripts/powermenu_t1"
M.clipmenu = "~/.config/rofi/cliphist/cliphist-menu"
M.killmenu = "~/.config/rofi/killmenu/kill-menu"

-- Modifier keys
M.mainMod = "SUPER"
M.alt = "ALT"
M.shift = "SHIFT"
M.ctrl = "CTRL"

-- Vim-style direction keys
M.vleft = "h"
M.vdown = "j"
M.vup = "k"
M.vright = "l"

M.full_mode_names = {
    [1] = "maximized",
    [2] = "fullscreen",
}

return M
