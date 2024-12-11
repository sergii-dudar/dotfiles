local wibox = require("wibox")
local vars = require("modules.variables")
local gears = require("gears")
local awful = require("awful")
local util = require("util.common-util")

local time = wibox.widget.textclock()
time.font = vars.font.widget

local clock_icon = util.to_span("󰔛 ", "#bd93f9", 16)
local formats = {
    clock_icon .. util.to_span("%I:%M %p", "#a6d189"),
    clock_icon .. util.to_span("%H:%M", "#a6d189"),
    clock_icon .. util.to_span("%Y-%m-%d %H:%M", "#a6d189"),
}
local current_format_index = 1

time.format = formats[current_format_index]

local date = wibox.widget.textclock()
date.font = vars.font.widget
date.format = util.to_span(" ", "#7c8377", 16) .. util.to_span("%a %b %d", "#6272a4")

date:buttons(gears.table.join(awful.button({}, 1, function()
    awful.spawn.with_shell("gnome-calendar")
end)))
time:buttons(gears.table.join(
    awful.button({}, 1, function()
        awful.spawn.with_shell("gnome-clocks")
    end),
    awful.button({}, 3, function()
        current_format_index = (current_format_index % #formats) + 1
        time.format = formats[current_format_index]
    end)
))

local M = {}

local bg_color = "#2E3440" --"#2D2A2E" --"#2B3339"
M.time = util.decore_with_background(time, bg_color)
M.date = util.decore_with_background(date, bg_color)

return M
