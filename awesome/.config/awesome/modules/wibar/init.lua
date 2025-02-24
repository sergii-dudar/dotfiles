local awful = require("awful")
local wibox = require("wibox")

local date_widgets = require("modules.wibar.clock-widget")
local layout_widget = require("modules.wibar.layout-widget")
local taglist_widget = require("modules.wibar.taglist-widget")
local runner_widget = require("modules.wibar.runner-widget")
local simple_widget = require("modules.wibar.simple-widget")
local keyboard_layout_widget = require("modules.wibar.keyboard-layout-widget")
local system_widget = require("modules.wibar.system-widget")
--local mpris_widget = require("modules.wibar.mpris-widget")
local volume_widget = require("modules.wibar.volume-widget")
local battery_widget = require("modules.wibar.battery-widget")
local weather_widget = require("modules.wibar.weather-widget")
local util = require("util.common-util")

local M = {}

---@param opts { keybind: {} }
M.setup = function(opts)
    -- {{{ Wibar

    awful.screen.connect_for_each_screen(function(s)
        -- screen edges padding
        --s.padding = -6

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

        if s.index == 1 then
            -- render for main screen

            s.mywibox:setup({
                layout = wibox.layout.align.horizontal,
                expand = "none",
                {
                    widget = wibox.container.margin,
                    -- bottom = 2,
                    -- top = 2,
                    {
                        widget = wibox.container.place,
                        layout = wibox.layout.fixed.horizontal,
                        -- spacing = 0,
                        util.decore_with_background_left(layout_widget.layoutbox_with_name(s)),
                        --simple_widget.separator,
                        simple_widget.build_bg_separator(2, 0),
                        runner_widget.left_all,
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
                                util.decore_with_background_right(system_widget.cpu_temp),
                                simple_widget.separator,
                                util.decore_with_background_right(system_widget.fs),
                                simple_widget.separator,

                                util.decore_with_background_right(weather_widget.weather),
                                simple_widget.separator,

                                util.decore_with_background_right(
                                    util.group_widgets(
                                        util.widget_margin(wibox.widget.systray(), 0, 6, 4, 4),
                                        runner_widget.powermenu
                                    ),
                                    nil,
                                    nil,
                                    3
                                ),
                            })),
                        },
                    },
                },
            })
        else
            -- bar for all second monitors
            s.mywibox:setup({
                layout = wibox.layout.align.horizontal,
                expand = "none",
                {
                    widget = wibox.container.margin,
                    -- bottom = 2,
                    -- top = 2,
                    {
                        widget = wibox.container.place,
                        layout = wibox.layout.fixed.horizontal,
                        -- spacing = 0,
                        util.decore_with_background_left(layout_widget.layoutbox_with_name(s)),
                        simple_widget.separator,
                        --simple_widget.build_bg_separator(2, 0),
                        simple_widget.tasklist(s, opts),
                    },
                },

                {
                    widget = wibox.container.place,
                    layout = wibox.layout.fixed.horizontal,
                    taglist_widget.setup(s, opts),
                },

                { -- Right widgets

                    widget = wibox.container.place,
                    h_align = "right",
                    {
                        widget = wibox.container.margin,
                        {
                            widget = wibox.container.place,
                            layout = wibox.layout.fixed.horizontal,
                            simple_widget.build_bg_separator(2, 0),
                            runner_widget.left_all,
                            simple_widget.separator,
                        },
                    },
                },
            })
        end
    end)
end

return M