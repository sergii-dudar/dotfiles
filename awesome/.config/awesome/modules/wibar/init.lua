local awful = require("awful")
local wibox = require("wibox")

local date_widgets = require("modules.wibar.clock-widget")
local layout_widget = require("modules.wibar.layout-widget")
local taglist_widget = require("modules.wibar.taglist-widget")
local appmenu_widget = require("modules.wibar.appmenu-widget")
local powermenu_widget = require("modules.wibar.powermenu-widget")
local settings_widget = require("modules.wibar.settings-widget")
local simple_widget = require("modules.wibar.simple-widget")
local keyboard_layout_widget = require("modules.wibar.keyboard-layout-widget")
local system_widget = require("modules.wibar.system-widget")
--local mpris_widget = require("modules.wibar.mpris-widget")
local volume_widget = require("modules.wibar.volume-widget")
local battery_widget = require("modules.wibar.battery-widget")
local util = require("util.common-util")

local M = {}

---@param opts { keybind: {} }
M.setup = function(opts)
    -- {{{ Wibar

    awful.screen.connect_for_each_screen(function(s)
        -- screen edges padding
        s.padding = -6
        -- or:
        -- s.padding = {
        --     left = -6,
        --     right = 0,
        --     top = 2,
        --     bottom = 19,
        -- }

        -- Add widgets to the wibox
        s.mywibox = awful.wibar({
            position = "top",
            screen = s,
            height = 35,
        })

        s.mywibox:setup({
            layout = wibox.layout.align.horizontal,
            expand = "none",
            {
                widget = wibox.container.margin,
                bottom = 2,
                top = 2,
                {
                    widget = wibox.container.place,
                    layout = wibox.layout.fixed.horizontal,
                    --spacing = 5,
                    util.decore_with_background_left(
                        util.group_widgets(
                            appmenu_widget.applications,
                            util.widget_margin(settings_widget.settings, 8, 0, 0, 0)
                        ),
                        nil,
                        12,
                        2
                    ),
                    simple_widget.separator_no_left,
                    --wibox.container.margin(launcher.mylauncher, 5, 0, 0, 0),
                    --separator,
                    util.decore_with_background_left(layout_widget.layoutbox_with_name(s)),
                    simple_widget.separator,

                    -- Create a promptbox for each screen
                    awful.widget.prompt(),
                    simple_widget.tasklist(s, opts),
                    --simple_widget.separator,
                },
            },

            {
                widget = wibox.container.place,
                layout = wibox.layout.fixed.horizontal,
                --spacing = 15,
                util.decore_with_background_center(date_widgets.date),
                simple_widget.space,
                taglist_widget.setup(s, opts),
                simple_widget.space,
                util.decore_with_background_center(date_widgets.time),
            },

            { -- Right widgets

                widget = wibox.container.place,
                h_align = "right",
                {
                    widget = wibox.container.margin,
                    --right = 5,
                    --bottom = 1,
                    --top = 1,
                    {
                        widget = wibox.container.place,
                        layout = wibox.layout.fixed.horizontal,
                        --spacing = 5,
                        table.unpack(util.concat_match_tables({
                            util.decore_with_background_right(keyboard_layout_widget.setup()),
                            simple_widget.separator,
                            util.decore_with_background_right(volume_widget.setup(opts)),
                            simple_widget.separator,
                        }, {
                            is_match = function()
                                return util.directory_exists("/sys/class/power_supply/BAT0")
                            end,
                            table = {
                                -- add this widgets only if battery present
                                util.decore_with_background_right(battery_widget.battery),
                                simple_widget.separator,
                            },
                        }, {
                            util.decore_with_background_right(system_widget.mem),
                            simple_widget.separator,
                            util.decore_with_background_right(system_widget.cpu),
                            simple_widget.separator,
                            util.decore_with_background_right(system_widget.fs),
                            simple_widget.separator,

                            --util.widget_margin(wibox.widget.systray(), 0, 0, 6, 6),
                            util.decore_with_background_right(
                                util.group_widgets(
                                    util.widget_margin(wibox.widget.systray(), 0, 0, 4, 4),
                                    util.widget_margin(powermenu_widget.powermenu, 10, 0, 0, 0)
                                ),
                                nil,
                                nil,
                                5
                            ),
                        })),
                    },
                },
            },
        })
    end)
end

return M
