local beautiful = require("beautiful")
local wibox = require("wibox")
local awful = require("awful")
local vars = require("modules.variables")
local util = require("util.common-util")
local gears = require("gears")

local M = {}

M.setup = function()
    -- Define the icon and styling
    local icon = "󰌌 " -- Nerd Font icon for keyboard (you can change this)
    local color = "#cba6f7" -- Text color for layouts

    -- Define a mapping for layouts to display names/icons
    local layout_names = {
        us = "🇺🇸 US",
        ua = "🇺🇦 UA",
    }

    -- Create a custom widget to display layout
    local custom_keyboard_widget = wibox.widget({
        {
            -- uncomment if want add additional icon
            -- {
            --     id = "icon",
            --     markup = string.format("<span font='%s' foreground='%s'>%s</span>", font, color, icon),
            --     widget = wibox.widget.textbox,
            -- },
            {
                id = "text",
                markup = "",
                widget = wibox.widget.textbox,
            },
            layout = wibox.layout.fixed.horizontal,
            spacing = 5, -- Space between icon and text
        },
        set_text = function(self, layout)
            local layout_display = layout_names[layout] or layout -- Default to raw layout if no mapping
            self:get_children_by_id("text")[1].markup = util.to_span(layout_display, color)
        end,
        layout = wibox.container.margin,
        margins = 0,
    })

    custom_keyboard_widget:buttons(gears.table.join(
        awful.button({}, 1, function()
            awful.spawn.with_shell(vars.run.change_language)
        end),
        awful.button({}, 3, function()
            awful.spawn.with_shell(vars.run.monkey_type)
        end)
    ))

    -- custom_keyboard_widget.widget:buttons(awful.util.table.join(awful.button({}, 1, function()
    --     awful.spawn.with_shell(string.format("bash %s/dotfiles/bin/change_language.sh", vars.path.home_dir))
    --     custom_keyboard_widget.update()
    -- end)))

    -- Function to fetch current layout
    local function get_current_layout()
        -- Use `xkb-switch` or `setxkbmap` to get the current layout
        local f = io.popen("setxkbmap -query | grep layout | awk '{print $2}'")
        local layout = f:read("*a"):gsub("%s+", "") -- Remove any whitespace
        f:close()
        return layout
    end

    -- Update the widget text whenever the layout changes
    awful.widget.keyboardlayout():connect_signal("widget::redraw_needed", function()
        local current_layout = get_current_layout() -- fetch current layout
        custom_keyboard_widget:set_text(current_layout)
    end)

    -- Initial update
    local current_layout = get_current_layout() -- Fetch current layout
    custom_keyboard_widget:set_text(current_layout)

    -- Add the widget to your wibar
    return custom_keyboard_widget
end

return M
