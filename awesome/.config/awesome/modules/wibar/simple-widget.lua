local wibox = require("wibox")
local vars = require("modules.variables")
local awful = require("awful")
local util = require("util.common-util")

local M = {}

local gcolor = "#7f849c"
local gseparator = "" --  󰇝  󰮾 󱋱

local function build_separator(icon, color)
    return wibox.widget({
        widget = wibox.widget.textbox,
        font = vars.font.widget,
        markup = util.to_span(icon, color),
        align = "center",
        valign = "center",
    })
end

-- M.separator = build_separator(" " .. gseparator .. " ", gcolor)
-- M.separator_no_left = build_separator(gseparator .. " ", gcolor)
-- M.separator_no_right = build_separator(" " .. gseparator, gcolor)

M.separator = util.widget_margin(build_separator(gseparator, gcolor), 10, 10, 0, 0)
M.separator_no_left = util.widget_margin(build_separator(gseparator, gcolor), 0, 10, 0, 0)
M.separator_no_right = util.widget_margin(build_separator(gseparator, gcolor), 10, 0, 0, 0)

M.space = wibox.widget({
    widget = wibox.widget.textbox,
    text = " ",
})

M.tasklist = function(s, opts)
    -- Create a tasklist widget
    return awful.widget.tasklist({
        font = vars.font.widget,
        screen = s,
        filter = awful.widget.tasklist.filter.currenttags,
        buttons = opts.keybind.tasklist_buttons,
    })
end

return M
