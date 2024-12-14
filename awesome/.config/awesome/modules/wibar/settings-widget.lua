local wibox = require("wibox")
local vars = require("modules.variables")
local gears = require("gears")
local awful = require("awful")
local util = require("util.common-util")

local M = {}

M.settings = wibox.widget({
    widget = wibox.widget.textbox,
    markup = util.to_span(" ", "#3071db", 19),
    align = "center",
    valign = "center",
    font = vars.font.to_size(17),
})
M.settings:buttons(gears.table.join(awful.button({}, 1, function()
    awful.spawn.with_shell(vars.run.gnome_settings)
end)))

return M
