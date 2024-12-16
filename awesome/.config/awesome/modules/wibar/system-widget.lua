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
    awful.util.spawn(vars.run.disc_usage)
end)))

M.cpu = cpu
M.mem = mem
M.fs = fs
return M
