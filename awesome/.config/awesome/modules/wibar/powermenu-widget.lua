local wibox = require("wibox")
local vars = require("modules.variables")
local gears = require("gears")
local awful = require("awful")
local util = require("util.common-util")

local M = {}

M.powermenu = wibox.widget({
    widget = wibox.widget.textbox,
    markup = util.to_span(" ", "#d35f5e"),
    align = "center",
    valign = "center",
    font = vars.font.widget,
})
M.powermenu:buttons(gears.table.join(awful.button({}, 1, function()
    awful.spawn.with_shell(vars.run.powermenu)
end)))

return M
