local lain = require("lain")
local vars = require("modules.variables")
local util = require("util.common-util")

local M = {}

local gray = "#94928F"
local markup = lain.util.markup

M.cpu = lain.widget.cpu({
    settings = function()
        local perc = string.format("%02d", cpu_now.usage)
        widget:set_markup(markup.font(vars.font.widget, markup(gray, util.to_span("  ", "#8caaee")) .. perc .. "%"))
    end,
})

M.mem = lain.widget.mem({
    settings = function()
        local perc = string.format("%02d", mem_now.perc)
        widget:set_markup(markup.font(vars.font.widget, markup(gray, util.to_span("  ", "#a6e3a1")) .. perc .. "%"))
    end,
})

M.fs = lain.widget.fs({
    settings = function()
        local perc = string.format("%02d", fs_now["/home"].percentage)
        widget:set_markup(
            markup.font(
                vars.font.widget,
                markup(gray, util.to_span("  ", "#e5c890") .. perc .. "% " .. util.to_span("SSD", "#e5c890"))
            )
        )
    end,
})

return M
