local wibox = require("wibox")
local vars = require("modules.variables")
local gears = require("gears")
local awful = require("awful")
local util = require("util.common-util")

--#region time
local time = wibox.widget.textclock()
time.font = vars.font.widget

local clock_icon = util.to_span("󰔛 ", "#bd93f9", 16)
local time_formats = {
    clock_icon .. util.to_span("%I:%M %p", "#a6d189"),
    clock_icon .. util.to_span("%H:%M", "#6272a4"),
}
local current_time_format_index = 1
time.format = time_formats[current_time_format_index]

time:buttons(gears.table.join(
    awful.button({}, 1, function()
        awful.spawn.with_shell(vars.run.gnome_clocks)
    end),
    awful.button({}, 3, function()
        current_time_format_index = (current_time_format_index % #time_formats) + 1
        time.format = time_formats[current_time_format_index]
    end)
))
--#endregion

--#region date
local date = wibox.widget.textclock()
date.font = vars.font.widget

local date_icon = util.to_span(" ", "#7c8377", 16)
local date_formats = {
    date_icon .. util.to_span("%A, %B %d", "#6272a4"),
    date_icon .. util.to_span("%A, %d-%m-%Y", "#a6d189"),
}
local current_date_format_index = 1
date.format = date_formats[current_date_format_index]

date:buttons(gears.table.join(
    awful.button({}, 1, function()
        awful.spawn.with_shell(vars.run.gnome_calendar)
    end),
    awful.button({}, 3, function()
        current_date_format_index = (current_date_format_index % #date_formats) + 1
        date.format = date_formats[current_date_format_index]
    end)
))

--#endregion

local M = {}

local bg_color = "#2E3440" --"#2D2A2E" --"#2B3339"
M.time = util.decore_with_background(time, bg_color)
M.date = util.decore_with_background(date, bg_color)

return M
