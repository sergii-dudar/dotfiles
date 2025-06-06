local wibox = require("wibox")
local vars = require("modules.variables")
local gears = require("gears")
local awful = require("awful")
local util = require("util.common-util")
--local lain = require("lain")
local calendar_widget = require("modules.wibar.ext.calendar")
local word_clock_widget = require("modules.wibar.ext.word-clock")

--#region time
local time = wibox.widget.textclock()
time.font = vars.font.widget

local clock_icon = util.to_span("󰔛 ", "#bd93f9", 17)
local time_formats = {
    clock_icon .. util.to_span("%I:%M", "#a6d189") .. util.vars.icon_widget_space .. util.to_span("%p", "#8caaee"),
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

--[[ local time_popup = wibox.widget.textclock()
time_popup.format = util.to_span("󰔛 ", "#bd93f9", 26) .. util.to_span("%H:%M:%S", "#6272a4", 24)
time_popup.refresh = 1

util.show_hower_timer_popup({
    -- widget_content = word_clock_widget({
    --     font = vars.font.to_size(25),
    --     accent_color = "#ff79c6",
    --     main_color = "#8be9fd",
    --     is_human_readable = true,
    --     with_spaces = false,
    --     military_time = true,
    -- }),
    widget_content = time_popup,
    target_widget = time,
    position = "right",
    margins = 20,
}) ]]
--#endregion

--#region date
local date = wibox.widget.textclock()
date.font = vars.font.widget

local date_icon = util.to_span(" ", "#7c8377", 17)
local date_formats = {
    -- date_icon .. util.to_span("%A, %B %d", "#6272a4"),
    date_icon
        .. util.vars.icon_widget_space
        .. util.to_span("%a,", "#6272a4")
        .. util.vars.icon_widget_space
        .. util.to_span("%b", "#6272a4")
        .. util.vars.icon_widget_space
        .. util.to_span("%d", "#6272a4"),

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

local calendar = calendar_widget({
    theme = "catppuccin",
    placement = "top",
    start_sunday = false,
    radius = 12,
    auto_hide = true,
    timeout = 1.5,
    -- with customized next/previous (see table above)
    previous_month_button = 1,
    next_month_button = 3,
})
local calendar_hower_timer = util.run_timer_callback(1, calendar.toggle)
date:connect_signal("mouse::enter", function()
    if not calendar.visible then
        calendar_hower_timer:start()
    end
end)
date:connect_signal("mouse::leave", function()
    calendar_hower_timer:stop()
    if calendar.visible then
        calendar.toggle()
    end
end)

local M = {}

M.time = time
M.date = date

return M
