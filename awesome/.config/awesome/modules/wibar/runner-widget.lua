local vars = require("modules.variables")
local awful = require("awful")
local util = require("util.common-util")
local gears = require("gears")
local launcher = require("modules.awesome-launcher")

local icons_dir = vars.current_theme_dir .. "/icons/"
local iconst_dir = icons_dir .. "test/"
local text_icon_color = "#3071db"
local runner_bg_hover = "#8caaee"
local red = "#d35f5e"

--local settings = util.to_text_icon_runner(" ", 19, "#3071db", vars.run.gnome_settings)
local powermenu = util.to_text_icon_runner(" ", vars.font.default_size, red, vars.run.powermenu)

--local awesome_icon = icons_dir .. "awesome_grey.png"
local applications = util.to_text_icon_runner(" ", 19, "#535D6C") -- "#bd93f9"
--local applications = util.to_imagebox_runner(awesome_icon, "awesome")
applications:buttons(gears.table.join(
    awful.button({}, 1, function()
        awful.spawn.with_shell(vars.run.mymenu)
    end),
    awful.button({}, 3, function()
        launcher.mymainmenu:toggle()
    end)
))

local kitty_icon = icons_dir .. "kitty.svg"
local wezterm_icon = icons_dir .. "wezterm.png"
local ghostty_icon = icons_dir .. "ghostty.png"
local pipette_icon = icons_dir .. "pipette.png"
local evince_icon = icons_dir .. "evince.png"
-- local chrome_icon = icons_dir .. "google-chrome.svg"
local brave_icon = icons_dir .. "brave.png"
local intellij_icon = icons_dir .. "intellij.svg"
local insomnia_icon = icons_dir .. "insomnia.png"
local torrent_icon = icons_dir .. "qbittorrent.svg"
local settings_icon = iconst_dir .. "settings4.png"

-- magick kitty.svg -resize 320x320 kitty_320.svg
local kitty_terminal = util.to_imagebox_runner(kitty_icon, "kitty")
local wezterm_terminal = util.to_imagebox_runner(wezterm_icon, "wezterm")
local ghostty_terminal = util.to_imagebox_runner(ghostty_icon, "ghostty")
local pipette = util.to_imagebox_runner(pipette_icon, "gpick")
-- local chrome = util.to_imagebox_runner(chrome_icon, "google-chrome-stable")
local brave = util.to_imagebox_runner(brave_icon, "~/dotfiles/bin/start-browser")
local intellij = util.to_imagebox_runner(intellij_icon, "intellij-idea-ultimate")
local insomnia = util.to_imagebox_runner(insomnia_icon, "/opt/insomnia/insomnia %U")
local torrent = util.to_imagebox_runner(torrent_icon, "env QT_SCALE_FACTOR=1.4 qbittorrent")
local settings = util.to_imagebox_runner(settings_icon, vars.run.gnome_settings)
local evince = util.to_imagebox_runner(evince_icon, "evince")

util.add_text_info_pupup({
    target_widget = applications,
    info_text = "Applications Menu",
    text_icon = " ",
    text_icon_fg = text_icon_color,
    text_icon_size = 19,
})
util.add_text_info_pupup({
    target_widget = settings,
    info_text = "Gnome Settings",
    text_icon = " ",
    text_icon_fg = text_icon_color,
    text_icon_size = 22,
})
util.add_text_info_pupup({
    target_widget = powermenu,
    info_text = "Power Menu",
    text_icon = " ",
    text_icon_fg = red,
    text_icon_size = 22,
    position = "right",
})

util.add_text_info_pupup({
    target_widget = kitty_terminal,
    info_text = "Kitty Terminal",
    icon_path = kitty_icon,
})
util.add_text_info_pupup({
    target_widget = wezterm_terminal,
    info_text = "Wezterm Terminal",
    icon_path = wezterm_icon,
})
util.add_text_info_pupup({
    target_widget = ghostty_terminal,
    info_text = "Ghostty Terminal",
    icon_path = ghostty_icon,
})
util.add_text_info_pupup({
    target_widget = pipette,
    info_text = "GPick App",
    icon_path = pipette_icon,
})
-- util.add_text_info_pupup({
--     target_widget = chrome,
--     info_text = "Google Chrome",
--     icon_path = chrome_icon,
-- })
util.add_text_info_pupup({
    target_widget = brave,
    info_text = "Brave Browser",
    icon_path = brave_icon,
})
util.add_text_info_pupup({
    target_widget = intellij,
    info_text = "Intellij Idea",
    icon_path = intellij_icon,
})
util.add_text_info_pupup({
    target_widget = insomnia,
    info_text = "Insomnia",
    icon_path = insomnia_icon,
})
util.add_text_info_pupup({
    target_widget = torrent,
    info_text = "qbittorrent",
    icon_path = torrent_icon,
})
util.add_text_info_pupup({
    target_widget = evince,
    info_text = "evince previwer",
    icon_path = evince_icon,
})

return {
    left_all = util.decore_with_background_left(
        util.group_widgets(
            util.add_bg_hover_to_widget(util.widget_margin(applications, 4, 4, 0, 0), runner_bg_hover),
            util.add_bg_hover_to_widget(util.widget_margin(settings, 4, 4, 0, 0), runner_bg_hover),
            util.add_bg_hover_to_widget(util.widget_margin(ghostty_terminal, 4, 4, 0, 0), runner_bg_hover),
            util.add_bg_hover_to_widget(util.widget_margin(kitty_terminal, 4, 4, 0, 0), runner_bg_hover),
            util.add_bg_hover_to_widget(util.widget_margin(wezterm_terminal, 4, 4, 0, 0), runner_bg_hover),
            --util.add_bg_hover_to_widget(util.widget_margin(chrome, 5, 5, 0, 0), runner_bg_hover),
            util.add_bg_hover_to_widget(util.widget_margin(brave, 5, 5, 0, 0), runner_bg_hover),
            util.add_bg_hover_to_widget(util.widget_margin(intellij, 5, 5, 0, 0), runner_bg_hover),
            util.add_bg_hover_to_widget(util.widget_margin(insomnia, 5, 5, 0, 0), runner_bg_hover),
            util.add_bg_hover_to_widget(util.widget_margin(pipette, 4, 4, 0, 0), runner_bg_hover),
            util.add_bg_hover_to_widget(util.widget_margin(torrent, 4, 4, 0, 0), runner_bg_hover),
            util.add_bg_hover_to_widget(util.widget_margin(evince, 4, 4, 0, 0), runner_bg_hover)
        ),
        nil,
        8,
        2
    ),
    powermenu = util.widget_margin(powermenu, 5, 5, 0, 0),
}