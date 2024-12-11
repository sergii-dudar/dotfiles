local beautiful = require("beautiful")
local awful = require("awful")
local hotkeys_popup = require("awful.hotkeys_popup")
local vars = require("modules.variables")

local M = {}
local awesome_icon = vars.current_theme_dir .. "/icons/awesome.png"

-- {{{ Menu
-- Create a launcher widget and a main menu
M.myawesomemenu = {
    {
        "hotkeys",
        function()
            hotkeys_popup.show_help(nil, awful.screen.focused())
        end,
    },
    { "manual", vars.app.terminal .. " -e man awesome" },
    { "edit config", vars.app.editor_cmd .. " " .. awesome.conffile },
    { "restart", awesome.restart },
    {
        "quit",
        function()
            awesome.quit()
        end,
    },
}
M.mymainmenu = awful.menu({
    items = {
        { "awesome", M.myawesomemenu, awesome_icon },
        { "open terminal", vars.app.terminal },
    },
})

M.mylauncher = awful.widget.launcher({
    image = awesome_icon,
    menu = M.mymainmenu,
    font = vars.font.widget,
})

-- Set the terminal for applications that require it
-- }}}

return M
