local wibox = require("wibox")
local vars = require("modules.variables")
local gears = require("gears")
local awful = require("awful")

local M = {}

M.powermenu = wibox.widget({
    widget = wibox.widget.textbox,
    markup = "<span foreground='#d35f5e'> </span>",
    align = "center",
    valign = "center",
    font = vars.font.widget,
})
M.powermenu:buttons(gears.table.join(awful.button({}, 1, function()
    awful.spawn.with_shell(vars.path.powermenu)
end)))

return M
