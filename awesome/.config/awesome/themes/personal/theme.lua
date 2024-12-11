---------------------------
-- Default awesome theme --
---------------------------

local theme_assets = require("beautiful.theme_assets")
local xresources = require("beautiful.xresources")
local rnotification = require("ruled.notification")
local beautiful = require("beautiful")
local vars = require("modules.variables")
local dpi = xresources.apply_dpi
local gears_shape = require("gears.shape")
local gears = require("gears")

--local gfs = require("gears.filesystem")
--local themes_path = gfs.get_themes_dir()
local themes_path = vars.current_theme_dir
--print(themes_path)

local theme = {}

theme.font = "CaskaydiaCove Nerd Font Bold 10"

theme.bg_normal = "#232634"
theme.bg_focus = "#535d6c"
--theme.bg_urgent = "#ff0000"
theme.bg_urgent = "#232634"
theme.bg_minimize = "#444444"
theme.bg_systray = theme.bg_normal
theme.systray_icon_spacing = dpi(5)
--theme.icon_theme = "Dracula"

theme.fg_normal = "#aaaaaa"
theme.fg_focus = "#ffffff"
--theme.fg_urgent = "#ffffff"
theme.fg_urgent = "#8caaee"
theme.fg_minimize = "#ffffff"

theme.useless_gap = dpi(5)
theme.border_width = dpi(3)
theme.border_color_normal = "#535d6c"
theme.border_color_active = "#80a0ff"
theme.border_color_marked = "#91231c"

theme.tasklist_fg_focus = "#c678dd"
theme.tasklist_fg_normal = "#f4b8e4"

-- There are other variable sets
-- overriding the default one when
-- defined, the sets are:
-- taglist_[bg|fg]_[focus|urgent|occupied|empty|volatile]
-- tasklist_[bg|fg]_[focus|urgent]
-- titlebar_[bg|fg]_[normal|focus]
-- tooltip_[font|opacity|fg_color|bg_color|border_width|border_color]
-- prompt_[fg|bg|fg_cursor|bg_cursor|font]
-- hotkeys_[bg|fg|border_width|border_color|shape|opacity|modifiers_fg|label_bg|label_fg|group_margin|font|description_font]
-- Example:
--theme.taglist_bg_focus = "#ff0000"

-- Generate taglist squares:
-- local taglist_square_size = dpi(4)
-- theme.taglist_squares_sel = theme_assets.taglist_squares_sel(taglist_square_size, theme.fg_normal)
-- theme.taglist_squares_unsel = theme_assets.taglist_squares_unsel(taglist_square_size, theme.fg_normal)

theme.taglist_fg_focus = "#a6d189"
theme.taglist_bg_focus = "#44475a"
theme.taglist_fg_empty = "#51576d"

--theme.taglist_bg_occupied = "#44475a"
theme.taglist_fg_occupied = "#8caaee"

theme.taglist_squares_sel = nil
theme.taglist_squares_unsel = nil

--theme.taglist_shape_border_width = 3
--theme.taglist_shape_border_color = "#f4b8e4"

--beautiful.taglist_font = "CaskaydiaCove Nerd Font Bold 18"
-- Variables set for theming notifications:
-- notification_font
-- notification_[bg|fg]
-- notification_[width|height|margin]
-- notification_[border_color|border_width|shape|opacity]

-- Variables set for theming the menu:
-- menu_[bg|fg]_[normal|focus]
-- menu_[border_color|border_width]
theme.menu_submenu_icon = themes_path .. "/submenu.png"
theme.menu_height = dpi(15)
theme.menu_width = dpi(100)

-- You can add as many variables as
-- you wish and access them by using
-- beautiful.variable in your rc.lua
--theme.bg_widget = "#cc0000"

-- Define the image to load
theme.titlebar_close_button_normal = themes_path .. "/titlebar/close_normal.png"
theme.titlebar_close_button_focus = themes_path .. "/titlebar/close_focus.png"

theme.titlebar_minimize_button_normal = themes_path .. "/titlebar/minimize_normal.png"
theme.titlebar_minimize_button_focus = themes_path .. "/titlebar/minimize_focus.png"

theme.titlebar_ontop_button_normal_inactive = themes_path .. "/titlebar/ontop_normal_inactive.png"
theme.titlebar_ontop_button_focus_inactive = themes_path .. "/titlebar/ontop_focus_inactive.png"
theme.titlebar_ontop_button_normal_active = themes_path .. "/titlebar/ontop_normal_active.png"
theme.titlebar_ontop_button_focus_active = themes_path .. "/titlebar/ontop_focus_active.png"

theme.titlebar_sticky_button_normal_inactive = themes_path .. "/titlebar/sticky_normal_inactive.png"
theme.titlebar_sticky_button_focus_inactive = themes_path .. "/titlebar/sticky_focus_inactive.png"
theme.titlebar_sticky_button_normal_active = themes_path .. "/titlebar/sticky_normal_active.png"
theme.titlebar_sticky_button_focus_active = themes_path .. "/titlebar/sticky_focus_active.png"

theme.titlebar_floating_button_normal_inactive = themes_path .. "/titlebar/floating_normal_inactive.png"
theme.titlebar_floating_button_focus_inactive = themes_path .. "/titlebar/floating_focus_inactive.png"
theme.titlebar_floating_button_normal_active = themes_path .. "/titlebar/floating_normal_active.png"
theme.titlebar_floating_button_focus_active = themes_path .. "/titlebar/floating_focus_active.png"

theme.titlebar_maximized_button_normal_inactive = themes_path .. "/titlebar/maximized_normal_inactive.png"
theme.titlebar_maximized_button_focus_inactive = themes_path .. "/titlebar/maximized_focus_inactive.png"
theme.titlebar_maximized_button_normal_active = themes_path .. "/titlebar/maximized_normal_active.png"
theme.titlebar_maximized_button_focus_active = themes_path .. "/titlebar/maximized_focus_active.png"

theme.wallpaper = themes_path .. "/background.png"

-- You can use your own layout icons like this:
theme.layout_fairh = themes_path .. "/layouts/fairh.png"
theme.layout_fairv = themes_path .. "/layouts/fairv.png"
theme.layout_floating = themes_path .. "/layouts/floating.png"
theme.layout_magnifier = themes_path .. "/layouts/magnifier.png"
theme.layout_max = themes_path .. "/layouts/max.png"
theme.layout_fullscreen = themes_path .. "/layouts/fullscreen.png"
theme.layout_tilebottom = themes_path .. "/layouts/tilebottom.png"
theme.layout_tileleft = themes_path .. "/layouts/tileleft.png"
theme.layout_tile = themes_path .. "/layouts/tile.png"
theme.layout_tiletop = themes_path .. "/layouts/tiletop.png"
theme.layout_spiral = themes_path .. "/layouts/spiral.png"
theme.layout_dwindle = themes_path .. "/layouts/dwindle.png"
theme.layout_cornernw = themes_path .. "/layouts/cornernw.png"
theme.layout_cornerne = themes_path .. "/layouts/cornerne.png"
theme.layout_cornersw = themes_path .. "/layouts/cornersw.png"
theme.layout_cornerse = themes_path .. "/layouts/cornerse.png"

-- Generate Awesome icon:
theme.awesome_icon = theme_assets.awesome_icon(theme.menu_height, theme.bg_focus, theme.fg_focus)

-- Define the icon theme for application icons. If not set then the icons
-- from /usr/share/icons and /usr/share/icons/hicolor will be used.
--theme.icon_theme = nil
theme.icon_theme = "Dracula"
--theme.icon_theme = "/usr/share/icons/Dracula"
--theme.icon_theme = "Adwaita"

-- Set different colors for urgent notifications.
rnotification.connect_signal("request::rules", function()
    rnotification.append_rule({
        rule = { urgency = "critical" },
        properties = { bg = "#ff0000", fg = "#ffffff" },
    })
end)

return theme

-- vim: filetype=lua:expandtab:shiftwidth=4:tabstop=8:softtabstop=4:textwidth=80
