local wibox = require("wibox")
local vars = require("modules.variables")
local gears = require("gears")
local awful = require("awful")
local launcher = require("modules.awesome-launcher")
local util = require("util.common-util")

local M = {}

M.separator = wibox.widget({
    widget = wibox.widget.textbox,
    font = vars.font.widget,
    markup = "<span foreground='#7f849c'> 󱋱 </span>",
    align = "center",
    valign = "center",
})
M.separator_no_left = wibox.widget({
    widget = wibox.widget.textbox,
    font = vars.font.widget,
    markup = "<span foreground='#7f849c'>󱋱 </span>",
    align = "center",
    valign = "center",
})
M.separator_no_right = wibox.widget({
    widget = wibox.widget.textbox,
    font = vars.font.widget,
    markup = "<span foreground='#7f849c'> 󱋱</span>",
    align = "center",
    valign = "center",
})
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
