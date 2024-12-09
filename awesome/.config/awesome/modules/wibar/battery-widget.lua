local wibox = require("wibox")
local vars = require("modules.variables")
local gears = require("gears")
local awful = require("awful")
local lain = require("lain")

local M = {}

local gray = "#94928F"
local markup = lain.util.markup

M.battery = lain.widget.bat({
    battery = "BAT0",
    settings = function()
        local perc = tonumber(bat_now.perc) or 0
        local icon = " "
        local icon_fg = "#a6d189"

        if perc <= 7 then
            icon_fg = "#ff5555"
            icon = " "
        elseif perc <= 20 then
            icon_fg = "#e78284"
            icon = " "
        elseif perc <= 40 then
            icon_fg = "#ef9f76"
            icon = " "
        elseif perc <= 60 then
            icon_fg = "#f1fa8c"
            icon = " "
        elseif perc <= 80 then
            icon_fg = "#a6d189"
            icon = " "
        elseif perc <= 100 then
            icon_fg = "#a6d189"
            icon = " "
        end

        if bat_now.ac_status == 1 then
            icon_fg = "#a6d189"
            icon = " "
        end

        widget:set_markup(
            markup.font(
                vars.font.widget,
                markup(gray, string.format("<span foreground='%s'>%s </span>", icon_fg, icon)) .. perc .. "%"
            )
        )
    end,
})

return M
