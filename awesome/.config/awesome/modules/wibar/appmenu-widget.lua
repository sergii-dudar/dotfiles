local wibox = require("wibox")
local vars = require("modules.variables")
local gears = require("gears")
local awful = require("awful")
local launcher = require("modules.awesome-launcher")

local M = {}

M.applications = wibox.widget({
    widget = wibox.widget.textbox,
    markup = "<span foreground='#bd93f9'>  </span>",
    --markup = "<span foreground='#61afef'>  </span>",
    align = "center",
    valign = "center",
    font = vars.font.to_size(17),
})
M.applications:buttons(gears.table.join(
    awful.button({}, 1, function()
        awful.spawn.with_shell(vars.path.mymenu)
    end),
    awful.button({}, 3, function()
        launcher.mymainmenu:toggle()
    end)
))

return M
