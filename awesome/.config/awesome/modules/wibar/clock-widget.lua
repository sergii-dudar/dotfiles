local wibox = require("wibox")
local vars = require("modules.variables")
local gears = require("gears")
local awful = require("awful")

local time = wibox.widget.textclock()
time.font = vars.font.widget
time.format =
    "<span foreground='#bd93f9' font='CaskaydiaCove Nerd Font Bold 15'>󰔛 </span><span foreground='#a6d189'>%I:%M %p</span>"
local date = wibox.widget.textclock()
date.font = vars.font.widget
date.format = "<span foreground='#7c8377'> </span><span foreground='#6272a4'>%a %b %d</span>"

date:buttons(gears.table.join(awful.button({}, 1, function()
    awful.spawn.with_shell("gnome-calendar")
end)))
time:buttons(gears.table.join(awful.button({}, 1, function()
    awful.spawn.with_shell("gnome-clocks")
end)))

local M = {}

M.time = time
M.date = date

return M
