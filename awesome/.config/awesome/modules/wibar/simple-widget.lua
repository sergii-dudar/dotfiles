local wibox = require("wibox")
local vars = require("modules.variables")
local awful = require("awful")
local util = require("util.common-util")

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

local function build_bg_separator(left, right)
    return util.widget_margin(
        wibox.widget({
            widget = wibox.widget.textbox,
            font = vars.font.widget,
            text = "",
            align = "center",
            valign = "center",
        }),
        left,
        right,
        0,
        0
    )
end

-- local separator = build_separator(" " .. gseparator .. " ", gcolor)
-- local separator_no_left = build_separator(gseparator .. " ", gcolor)
-- local separator_no_right = build_separator(" " .. gseparator, gcolor)

-- local separator = util.widget_margin(build_separator(gseparator, gcolor), 10, 10, 0, 0)
-- local separator_no_left = util.widget_margin(build_separator(gseparator, gcolor), 0, 10, 0, 0)
-- local separator_no_right = util.widget_margin(build_separator(gseparator, gcolor), 10, 0, 0, 0)

local bg_separator = build_bg_separator(3, 0)
local bg_separator_no_left = build_bg_separator(3, 0)
local bg_separator_no_right = build_bg_separator(3, 0)

local space = wibox.widget({
    widget = wibox.widget.textbox,
    text = " ",
})

local tasklist = function(s, opts)
    -- Create a tasklist widget
    return util.group_widgets(
        wibox.widget({
            widget = wibox.widget.textbox,
            markup = util.to_span("󰑮: ", "#d183e8", 20),
            align = "center",
            valign = "center",
        }),
        awful.widget.tasklist({
            font = vars.font.widget,
            screen = s,
            filter = awful.widget.tasklist.filter.currenttags,
            buttons = opts.keybind.tasklist_buttons,
        })
    )
end

return {
    tasklist = tasklist,
    space = space,
    -- separator = separator,
    -- separator_no_left = separator_no_left,
    -- separator_no_right = separator_no_right,
    separator = bg_separator,
    separator_no_left = bg_separator_no_left,
    separator_no_right = bg_separator_no_right,
}
