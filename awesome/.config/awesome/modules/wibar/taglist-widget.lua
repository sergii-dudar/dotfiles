local wibox = require("wibox")
local vars = require("modules.variables")
local gears = require("gears")
local awful = require("awful")

local M = {}

M.tags = { "1 ", "2 ", "3 ", "4 ", "5 ", "6 󰣇", "7 ", "8 ", "9 " }

M.setup = function(s, opts)
    -- Each screen has its own tag table.
    --awful.tag({ "1", "2", "3", "4", "5", "6", "7", "8", "9" }, s, awful.layout.layouts[1])
    awful.tag(
        --{ "1 ", "2 ", "3 ", "4 ", "5 ", "6 󰣇", "7 ", "8 ", "9 " },
        M.tags,
        s,
        awful.layout.layouts[1]
    )

    -- Create a taglist widget
    local change_border_bottom_callback = function(self, tag, index, objects)
        local tag_selected_border_color = "#f4b8e4"
        local tag_with_clients_border_color = "#6272a4"
        local tag_inactive_border_color = "#3b4252"
        local border = self:get_children_by_id("bottom_border")[1]
        if tag.selected then
            border.bg = tag_selected_border_color
        elseif #tag:clients() > 0 then
            border.bg = tag_with_clients_border_color
        else
            border.bg = tag_inactive_border_color
        end
    end

    return awful.widget.taglist({
        screen = s,
        filter = awful.widget.taglist.filter.all,
        buttons = opts.keybind.taglist_buttons,
        style = {
            font = vars.font.widget,
            spacing = 0,
        },
        widget_template = {
            {
                {
                    {
                        id = "text_role",
                        widget = wibox.widget.textbox, -- Display the tag name
                        forced_width = 57,
                        forced_height = 30,
                        align = "center",
                        halign = "center",
                        valign = "center",
                    },
                    margins = { top = 0, bottom = 0, left = 0, right = 4 },
                    widget = wibox.container.margin, -- Wrap the widget in a margin container
                },
                id = "background_role",
                widget = wibox.container.background, -- Background color changes for active/urgent tags
                --forced_height = 30,
            },
            {
                id = "bottom_border",
                forced_height = 4, -- Height of the bottom border
                --bg = "#f4b8e4", -- Border color
                bg = "#00000000", -- Default: transparent
                widget = wibox.container.background,
            },
            layout = wibox.layout.fixed.vertical, -- Stack the content and border vertically
            create_callback = change_border_bottom_callback,
            update_callback = change_border_bottom_callback,
        },
    })
end

return M
