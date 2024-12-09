local lain = require("lain")
local vars = require("modules.variables")

local M = {}

local gray = "#94928F"
local markup = lain.util.markup

M.cpu = lain.widget.cpu({
    settings = function()
        local perc = string.format("%2d", cpu_now.usage)
        widget:set_markup(
            markup.font(vars.font.widget, markup(gray, "<span foreground='#8caaee'>  </span>") .. perc .. "%")
        )
    end,
})

M.mem = lain.widget.mem({
    settings = function()
        local perc = string.format("%2d", mem_now.perc)
        widget:set_markup(
            markup.font(vars.font.widget, markup(gray, "<span foreground='#a6e3a1'>  </span>") .. perc .. "%")
        )
    end,
})

M.fs = lain.widget.fs({
    settings = function()
        local perc = string.format("%2d", fs_now["/home"].percentage)
        widget:set_markup(
            markup.font(
                vars.font.widget,
                markup(gray, "<span foreground='#e5c890'>  </span>")
                    .. perc
                    .. "% <span foreground='#e5c890'>SSD</span>"
            )
        )
    end,
})

return M
