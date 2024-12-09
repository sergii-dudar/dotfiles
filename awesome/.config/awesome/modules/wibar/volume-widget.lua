local wibox = require("wibox")
local vars = require("modules.variables")
local gears = require("gears")
local awful = require("awful")
local lain = require("lain")

local gray = "#94928F"
local markup = lain.util.markup

local M = {}

M.volume = lain.widget.alsa({
    settings = function()
        local icon = "<span foreground='#ca9ee6'>  </span>"
        if volume_now.status == "off" then
            icon = "<span foreground='#d35f5e'>  </span>"
        elseif tonumber(volume_now.level) == 0 then
            icon = "<span foreground='#ca9ee6'>  </span>"
        elseif tonumber(volume_now.level) <= 10 then
            icon = "<span foreground='#ca9ee6'>  </span>"
        end

        --widget:set_markup(markup.font(theme.font, " " .. volume_now.level .. "% "))
        widget:set_markup(markup.font(vars.font.widget, markup(gray, icon .. volume_now.level .. "%")))
    end,
})
-- awful.widget.watch("amixer get Master", 1, function(widget, stdout)
--     M.volume.update()
-- end)
M.volume.widget:buttons(awful.util.table.join(
    awful.button({}, 1, function()
        os.execute(string.format("%s set %s toggle", M.volume.cmd, M.volume.togglechannel or M.volume.channel))
        M.volume.update()
    end),
    awful.button({}, 4, function()
        awful.util.spawn("amixer set Master 1%+")
        M.volume.update()
    end),
    awful.button({}, 5, function()
        awful.util.spawn("amixer set Master 1%-")
        M.volume.update()
    end)
))

return M
