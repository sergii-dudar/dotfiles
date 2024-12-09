local beautiful = require("beautiful")
local wibox = require("wibox")
local awful = require("awful")
local vars = require("modules.variables")

local M = {}

M.setup = function()
    -- Define the icon and styling
    local icon = "󰌌 " -- Nerd Font icon for keyboard (you can change this)
    local font = vars.font.widget -- Font for the widget
    local color = "#ff9800" -- Text color for layouts

    -- Create the keyboard layout widget
    local keyboardlayout = awful.widget.keyboardlayout()

    -- Define a mapping for layouts to display names/icons
    local layout_names = {
        -- us = "US",
        -- ua = "UA",
        us = "US 🇺🇸",
        ua = "UA 🇺🇦",
    }

    -- Create a custom widget to display layout
    local custom_keyboard_widget = wibox.widget({
        {
            {
                id = "icon",
                markup = string.format("<span font='%s' foreground='%s'>%s</span>", font, color, icon),
                widget = wibox.widget.textbox,
            },
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
            self:get_children_by_id("text")[1].markup =
                string.format("<span font='%s' foreground='%s'>%s</span>", font, color, layout_display)
        end,
        layout = wibox.container.margin,
        margins = 5,
    })

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
        local current_layout = get_current_layout() -- Fetch current layout
        custom_keyboard_widget:set_text(current_layout)
    end)

    -- Initial update
    local current_layout = get_current_layout() -- Fetch current layout
    custom_keyboard_widget:set_text(current_layout)

    -- Add the widget to your wibar
    return custom_keyboard_widget
end

return M
