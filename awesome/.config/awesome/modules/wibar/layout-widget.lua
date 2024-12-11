local wibox = require("wibox")
local gears = require("gears")
local awful = require("awful")
local util = require("util.common-util")

local M = {}

M.layoutbox_with_name = function(s)
    local layoutbox = awful.widget.layoutbox(s)
    local layout_name = wibox.widget({
        align = "center",
        valign = "center",
        widget = wibox.widget.textbox,
    })

    -- Update the layout name whenever the layout changes
    local function update_layout_name()
        local current_layout = awful.layout.getname(awful.layout.get(s))
        layout_name.markup = util.to_span("[" .. string.upper(current_layout) .. "]", "#d183e8", 13)
    end

    -- Connect signals to update on layout change
    awful.tag.attached_connect_signal(s, "property::selected", update_layout_name)
    awful.tag.attached_connect_signal(s, "property::layout", update_layout_name)
    awesome.connect_signal("layout::changed", update_layout_name)

    -- Update on startup
    update_layout_name()

    -- Combine the layoutbox and name
    local widget = wibox.widget({
        layoutbox,
        layout_name,
        spacing = 5,
        layout = wibox.layout.fixed.horizontal,
    })

    widget:buttons(gears.table.join(
        awful.button({}, 1, function()
            awful.layout.inc(1)
        end),
        awful.button({}, 3, function()
            awful.layout.inc(-1)
        end),
        awful.button({}, 4, function()
            awful.layout.inc(1)
        end),
        awful.button({}, 5, function()
            awful.layout.inc(-1)
        end)
    ))

    return widget
end

return M