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

