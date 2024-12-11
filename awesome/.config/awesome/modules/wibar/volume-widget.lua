local vars = require("modules.variables")
local gears = require("gears")
local awful = require("awful")
local lain = require("lain")
local util = require("util.common-util")

local gray = "#94928F"
local markup = lain.util.markup

local M = {}

---@param opts { keybind: {} }
M.setup = function(opts)
    local volume = lain.widget.alsa({
        settings = function()
            local icon = util.to_span("  ", "#ca9ee6")
            if volume_now.status == "off" then
                icon = util.to_span("  ", "#d35f5e")
            elseif tonumber(volume_now.level) == 0 then
                icon = util.to_span("  ", "#ca9ee6")
            elseif tonumber(volume_now.level) <= 10 then
                icon = util.to_span("  ", "#ca9ee6")
            end

            --widget:set_markup(markup.font(theme.font, " " .. volume_now.level .. "% "))
            widget:set_markup(markup.font(vars.font.widget, markup(gray, icon .. volume_now.level .. "%")))
        end,
    })
    -- awful.widget.watch("amixer get Master", 1, function(widget, stdout)
    --     M.volume.update()
    -- end)
    volume.widget:buttons(awful.util.table.join(
        awful.button({}, 1, function()
            os.execute(string.format("%s set %s toggle", volume.cmd, volume.togglechannel or volume.channel))
            volume.update()
        end),
        awful.button({}, 4, function()
            awful.util.spawn("amixer set Master 1%+")
            volume.update()
        end),
        awful.button({}, 5, function()
            awful.util.spawn("amixer set Master 1%-")
            volume.update()
        end)
    ))

    -- the most fast and effective with live UI feedback way
    opts.keybind.globalkeys = gears.table.join(
        opts.keybind.globalkeys,
        awful.key({ vars.key.modkey }, "Left", function()
            os.execute(string.format("amixer set %s 5%%-", volume.channel))
            volume.update()
        end),
        awful.key({ vars.key.modkey }, "Right", function()
            os.execute(string.format("amixer set %s 5%%+", volume.channel))
            volume.update()
        end),
        awful.key({ vars.key.modkey }, "Down", function()
            os.execute(string.format("amixer set %s toggle", volume.togglechannel or volume.channel))
            volume.update()
        end)
    )
    return volume
end

return M
