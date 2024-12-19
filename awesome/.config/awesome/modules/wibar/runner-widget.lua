local wibox = require("wibox")
local vars = require("modules.variables")
local awful = require("awful")
local util = require("util.common-util")
local gears = require("gears")
local launcher = require("modules.awesome-launcher")

local settings = util.to_text_icon_runner(" ", 19, "#3071db", vars.run.gnome_settings)
local powermenu = util.to_text_icon_runner(" ", vars.font.default_size, "#d35f5e", vars.run.powermenu)

local applications = util.to_text_icon_runner(" ", 17, "#bd93f9")
applications:buttons(gears.table.join(
    awful.button({}, 1, function()
        awful.spawn.with_shell(vars.run.mymenu)
    end),
    awful.button({}, 3, function()
        launcher.mymainmenu:toggle()
    end)
))

local icons_dir = vars.current_theme_dir .. "/icons/"
local kitty_terminal = util.to_imagebox_runner(icons_dir .. "kitty.svg", "kitty")
local wezterm_terminal = util.to_imagebox_runner(icons_dir .. "wezterm.png", "wezterm")
local pipette = util.to_imagebox_runner(icons_dir .. "pipette2.png", "gpick")
local chrome = util.to_imagebox_runner(icons_dir .. "google-chrome.svg", "google-chrome-stable")
local intellij = util.to_imagebox_runner(icons_dir .. "intellij-idea.svg", "intellij-idea-ultimate")

local popup = awful.popup({
    widget = {
        {
            {
                id = "text_role",
                widget = wibox.widget.textbox,
                text = "Custom hover text!", -- Default text
            },
            {
                widget = wibox.widget.imagebox,
                image = icons_dir .. "kitty.svg", -- Use your icon here
                resize = true,
                forced_width = 16,
                forced_height = 16,
            },
            layout = wibox.layout.fixed.horizontal,
            spacing = 8,
        },
        margins = 10,
        widget = wibox.container.margin,
    },
    ontop = true,
    visible = false, -- Hidden by default
    placement = awful.placement.top, -- Show above the widget
    shape = function(cr, width, height)
        gears.shape.rounded_rect(cr, width, height, 5)
    end,
    bg = "#222222",
    fg = "#ffffff",
})

-- Add mouse enter/leave signals to show/hide the popup
settings:connect_signal("mouse::enter", function()
    --popup.widget:get_children_by_id("text_role")[1].text = "Hovered over widget!" -- Dynamic text
    local mouse_coords = mouse.coords()
    popup.x = mouse_coords.x - 15
    popup.y = mouse_coords.y + 15
    popup.visible = true
end)

settings:connect_signal("mouse::leave", function()
    popup.visible = false
end)

local runner_bg_hover = "#8caaee"
return {
    left_all = util.decore_with_background_left(
        util.group_widgets(
            util.add_bg_hover_to_widget(util.widget_margin(applications, 4, 4, 0, 0), runner_bg_hover),
            util.add_bg_hover_to_widget(util.widget_margin(settings, 4, -3, 0, 0), runner_bg_hover),
            util.add_bg_hover_to_widget(util.widget_margin(kitty_terminal, 4, 4, 0, 0), runner_bg_hover),
            util.add_bg_hover_to_widget(util.widget_margin(wezterm_terminal, 4, 4, 0, 0), runner_bg_hover),
            util.add_bg_hover_to_widget(util.widget_margin(chrome, 5, 5, 0, 0), runner_bg_hover),
            util.add_bg_hover_to_widget(util.widget_margin(intellij, 5, 5, 0, 0), runner_bg_hover),
            util.add_bg_hover_to_widget(util.widget_margin(pipette, 4, 4, 0, 0), runner_bg_hover)
        ),
        nil,
        8,
        2
    ),
    powermenu = powermenu,
}

