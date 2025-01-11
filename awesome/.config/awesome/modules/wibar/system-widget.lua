local lain = require("lain")
local vars = require("modules.variables")
local util = require("util.common-util")
local awful = require("awful")

local M = {}

local gray = "#94928F"
local markup = lain.util.markup

local cpu = lain.widget.cpu({
    settings = function()
        local perc = string.format("%02d", cpu_now.usage)
        --widget:set_markup(markup.font(vars.font.widget, markup(gray, util.to_span("  ", "#8caaee")) .. perc .. "%"))
        widget:set_markup(
            markup.font(
                vars.font.widget,
                markup(gray, util.to_span(" ", "#8caaee") .. util.to_icon_widget_space(2) .. perc .. "%")
            )
        )
    end,
})
cpu.widget:buttons(awful.util.table.join(awful.button({}, 1, function()
    awful.util.spawn(vars.run.gnome_system_monitor)
end)))

local mem = lain.widget.mem({
    settings = function()
        local perc = string.format("%02d", mem_now.perc)
        widget:set_markup(
            markup.font(
                vars.font.widget,
                markup(gray, util.to_span(" ", "#a6e3a1") .. util.vars.icon_widget_space .. perc .. "%")
            )
        )
    end,
})
mem.widget:buttons(awful.util.table.join(awful.button({}, 1, function()
    awful.util.spawn(vars.run.htop)
end)))

local fs = lain.widget.fs({
    settings = function()
        local perc = string.format("%02d", fs_now["/home"].percentage)
        widget:set_markup(
            markup.font(
                vars.font.widget,
                markup(
                    gray,
                    util.to_span(" ", "#e5c890")
                        .. util.vars.icon_widget_space
                        .. perc
                        .. "%"
                        .. util.vars.icon_widget_space
                        .. util.to_span("SSD", "#6272a4")
                )
            )
        )
    end,
})
fs.widget:buttons(awful.util.table.join(awful.button({}, 1, function()
    awful.util.spawn(vars.run.disc_gdu)
end)))

local function calculate_average(tbl)
    local sum = 0
    local count = 0

    for key, value in pairs(tbl) do
        -- Convert the value to a number if it's a numeric string
        local num = type(value) == "number" and value or tonumber(value)

        -- If it's a valid number, include it in the sum and count
        if num then
            sum = sum + num
            count = count + 1
        end
    end

    local average = count > 0 and (sum / count) or 0
    -- return sum, count, average
    return average
end

local cpu_temp = lain.widget.temp({
    settings = function()
        --local value = string.format("%02d", coretemp_now)
        --print("coretemp_now widget: " .. coretemp_now)
        --print("temp_now widget: " .. util.system.table_to_string(temp_now))
        local average_ft = string.format("%02d", calculate_average(temp_now))
        --print("average temp_now widget: " .. averate_ft)

        local perc = tonumber(coretemp_now) or 0
        local icon_fg = "#8caaee"
        local icon = ""

        if perc <= 25 then
            icon_fg = "#8caaee"
            icon = ""
        elseif perc <= 50 then
            icon_fg = "#8caaee"
            icon = ""
        elseif perc <= 65 then
            icon_fg = "#e5c890"
            icon = ""
        elseif perc <= 80 then
            icon_fg = "#e78284"
            icon = ""
        elseif perc <= 100 then
            icon_fg = "#ff5555"
            icon = ""
        end

        widget:set_markup(
            markup.font(
                vars.font.widget,
                markup(
                    gray,
                    util.to_span(icon, icon_fg)
                        .. util.vars.icon_widget_space
                        .. average_ft
                        .. markup.fontfg(vars.font.default, "#6272a4", "°C")
                )
            )
        )
    end,
})

-- local sysload = lain.widget.sysload({
--     settings = function()
--         widget:set_markup(markup.font(theme.font, markup(gray, " Cpu ") .. load_1 .. " "))
--     end
-- })

M.cpu = cpu
M.mem = mem
M.fs = fs
M.cpu_temp = cpu_temp
return M
