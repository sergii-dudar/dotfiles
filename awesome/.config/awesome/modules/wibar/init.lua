-- Standard awesome library
local gears = require("gears")
local awful = require("awful")
local launcher = require("modules.awesome-launcher")
local lain = require("lain")
local vars = require("modules.variables")
local wibox = require("wibox")

local M = {}

-- Create a layoutbox widget with layout name
local layoutbox_with_name = function(s)
    local layoutbox = awful.widget.layoutbox(s)
    local layout_name = wibox.widget({
        align = "center",
        valign = "center",
        widget = wibox.widget.textbox,
    })

    -- Update the layout name whenever the layout changes
    local function update_layout_name()
        local current_layout = awful.layout.getname(awful.layout.get(s))
        -- layout_name.text
        layout_name.markup = "[<span weight='bold' foreground='#d183e8'>" .. string.upper(current_layout) .. "</span>]"
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

---@param opts { keybind: {} }
M.setup = function(opts)
    -- Keyboard map indicator and switcher
    local keyboardlayout = awful.widget.keyboardlayout()

    -- {{{ Wibar
    -- Create a textclock widget
    local textclock = wibox.widget.textclock()
    textclock.font = "CaskaydiaCove Nerd Font Bold 13"
    textclock.format = "<span foreground='#6272a4'> %a %b %d</span><span foreground='#a6d189'> 󰔛 %I:%M %p</span>"

    awful.screen.connect_for_each_screen(function(s)
        -- Wallpaper
        --set_wallpaper(s)

        -- Each screen has its own tag table.
        awful.tag({ "1", "2", "3", "4", "5", "6", "7", "8", "9" }, s, awful.layout.layouts[1])

        -- Create a promptbox for each screen
        local promptbox = awful.widget.prompt()
        -- Create an imagebox widget which will contain an icon indicating which layout we're using.
        -- We need one layoutbox per screen.
        local layoutbox = layoutbox_with_name(s)

        -- Create a taglist widget
        local taglist = awful.widget.taglist({
            screen = s,
            filter = awful.widget.taglist.filter.all,
            buttons = opts.keybind.taglist_buttons,
            style = {
                font = "CaskaydiaCove Nerd Font Bold 13",
                spacing = 7,
            },
        })

        -- Create a tasklist widget
        local tasklist = awful.widget.tasklist({
            screen = s,
            filter = awful.widget.tasklist.filter.currenttags,
            buttons = opts.keybind.tasklist_buttons,
        })

        local separator = wibox.widget({
            widget = wibox.widget.textbox,
            markup = "<span foreground='#7f849c'> 󱋱 </span>",
            align = "center",
            valign = "center",
        })
        local separator_no_left = wibox.widget({
            widget = wibox.widget.textbox,
            markup = "<span foreground='#7f849c'>󱋱 </span>",
            align = "center",
            valign = "center",
        })
        local separator_no_right = wibox.widget({
            widget = wibox.widget.textbox,
            markup = "<span foreground='#7f849c'> 󱋱</span>",
            align = "center",
            valign = "center",
        })
        local space = wibox.widget({
            widget = wibox.widget.textbox,
            text = " ",
        })

        local applications = wibox.widget({
            widget = wibox.widget.textbox,
            markup = "<span foreground='#61afef'>  </span>",
            align = "center",
            valign = "center",
            font = "CaskaydiaCove Nerd Font Bold 13",
        })
        applications:buttons(gears.table.join(
            awful.button({}, 1, function()
                awful.spawn.with_shell(vars.path.mymenu)
            end),
            awful.button({}, 3, function()
                launcher.mymainmenu:toggle()
            end)
        ))

        local powermenu = wibox.widget({
            widget = wibox.widget.textbox,
            markup = "<span foreground='#d35f5e'> </span>",
            align = "center",
            valign = "center",
            font = "CaskaydiaCove Nerd Font Bold 13",
        })
        powermenu:buttons(gears.table.join(awful.button({}, 1, function()
            awful.spawn.with_shell(vars.path.powermenu)
        end)))

        local volume_widget = require("awesome-wm-widgets.volume-widget.volume")
        --sudo pacman -S acpi
        --local batteryarc_widget = require("awesome-wm-widgets.batteryarc-widget.batteryarc")
        local battery_widget = require("awesome-wm-widgets.battery-widget.battery")

        -- Add widgets to the wibox
        awful
            .wibar({
                position = "top",
                screen = s,
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
                        applications,
                        space,
                        --wibox.container.margin(launcher.mylauncher, 5, 0, 0, 0),
                        --separator,
                        layoutbox,
                        separator,
                        promptbox,
                        tasklist,
                    },
                },

                {
                    widget = wibox.container.place,
                    layout = wibox.layout.fixed.horizontal,
                    --spacing = 15,
                    taglist,
                    separator,
                    textclock,
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

                            keyboardlayout,
                            -- lain.widget.cpu({
                            --     settings = function()
                            --         widget:set_markup("Cpu " .. cpu_now.usage)
                            --     end,
                            -- }).widget,
                            separator,
                            volume_widget({}),
                            separator,
                            battery_widget({
                                path_to_icons = vars.path.home_dir
                                    .. "/.config/awesome/awesome-wm-widgets/battery-widget/icons/",
                            }),
                            separator,

                            --wibox.container.margin(my_widget, left, right, top, bottom)
                            wibox.container.margin(wibox.widget.systray(), 0, 0, 3, 3),
                            wibox.container.margin(powermenu, 10, 0, 0, 0),
                        },
                    },
                },
            })
    end)
end

return M
