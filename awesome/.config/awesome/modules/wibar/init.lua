-- Standard awesome library
local gears = require("gears")
local awful = require("awful")
local launcher = require("modules.awesome-launcher")
local lain = require("lain")
local wibox = require("wibox")

local date_widgets = require("modules.wibar.clock-widget")
local layout_widget = require("modules.wibar.layout-widget")
local taglist_widget = require("modules.wibar.taglist-widget")
local appmenu_widget = require("modules.wibar.appmenu-widget")
local powermenu_widget = require("modules.wibar.powermenu-widget")
local simple_widget = require("modules.wibar.simple-widget")
local cpu_widget = require("modules.wibar.cpu-widget")
local vars = require("modules.variables")

local M = {}

---@param opts { keybind: {} }
M.setup = function(opts)
    -- {{{ Wibar

    awful.screen.connect_for_each_screen(function(s)
        local volume_widget = require("awesome-wm-widgets.volume-widget.volume")
        --sudo pacman -S acpi
        --local batteryarc_widget = require("awesome-wm-widgets.batteryarc-widget.batteryarc")
        local battery_widget = require("awesome-wm-widgets.battery-widget.battery")

        -- Add widgets to the wibox
        awful
            .wibar({
                position = "top",
                screen = s,
                height = 35,
            })
            :setup({
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
                        appmenu_widget.applications,
                        simple_widget.space,
                        --wibox.container.margin(launcher.mylauncher, 5, 0, 0, 0),
                        --separator,
                        layout_widget.layoutbox_with_name(s),
                        simple_widget.separator,

                        -- Create a promptbox for each screen
                        awful.widget.prompt(),
                        simple_widget.tasklist(s, opts),
                    },
                },

                {
                    widget = wibox.container.place,
                    layout = wibox.layout.fixed.horizontal,
                    --spacing = 15,
                    date_widgets.date,
                    simple_widget.separator,
                    taglist_widget.setup(s, opts),
                    simple_widget.separator,
                    date_widgets.time,
                },

                { -- Right widgets

                    widget = wibox.container.place,
                    h_align = "right",
                    {
                        widget = wibox.container.margin,
                        right = 5,
                        --bottom = 1,
                        --top = 1,
                        {
                            widget = wibox.container.place,
                            layout = wibox.layout.fixed.horizontal,
                            --spacing = 5,

                            simple_widget.keyboardlayout,
                            simple_widget.separator,
                            cpu_widget,

                            simple_widget.separator,
                            volume_widget({}),
                            simple_widget.separator,
                            battery_widget({
                                path_to_icons = vars.path.home_dir
                                    .. "/.config/awesome/awesome-wm-widgets/battery-widget/icons/",
                            }),
                            simple_widget.separator,

                            --wibox.container.margin(my_widget, left, right, top, bottom)
                            wibox.container.margin(wibox.widget.systray(), 0, 0, 6, 6),
                            wibox.container.margin(powermenu_widget.powermenu, 10, 0, 0, 0),
                        },
                    },
                },
            })
    end)
end

return M
