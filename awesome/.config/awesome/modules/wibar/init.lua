-- Standard awesome library
local gears = require("gears")
local awful = require("awful")
local launcher = require("modules.awesome-launcher")
local lain = require("lain")

local M = {}

---@param opts { keybind: {} }
M.setup = function(opts)
    -- Widget and layout library
    local wibox = require("wibox")

    -- Keyboard map indicator and switcher
    local mykeyboardlayout = awful.widget.keyboardlayout()

    -- {{{ Wibar
    -- Create a textclock widget
    local mytextclock = wibox.widget.textclock()

    awful.screen.connect_for_each_screen(function(s)
        -- Wallpaper
        --set_wallpaper(s)

        -- Each screen has its own tag table.
        awful.tag({ "1", "2", "3", "4", "5", "6", "7", "8", "9" }, s, awful.layout.layouts[1])

        -- Create a promptbox for each screen
        s.mypromptbox = awful.widget.prompt()
        -- Create an imagebox widget which will contain an icon indicating which layout we're using.
        -- We need one layoutbox per screen.
        s.mylayoutbox = awful.widget.layoutbox(s)
        s.mylayoutbox:buttons(gears.table.join(
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
        -- Create a taglist widget
        s.mytaglist = awful.widget.taglist({
            screen = s,
            filter = awful.widget.taglist.filter.all,
            buttons = opts.keybind.taglist_buttons,
        })

        -- Create a tasklist widget
        s.mytasklist = awful.widget.tasklist({
            screen = s,
            filter = awful.widget.tasklist.filter.currenttags,
            buttons = opts.keybind.tasklist_buttons,
        })

        -- Create the wibox
        s.mywibox = awful.wibar({ position = "top", screen = s })

        local theme = { font = "Terminus 10.5" }
        local markup = lain.util.markup
        local volume = lain.widget.alsa({
            --togglechannel = "IEC958,3",
            settings = function()
                header = " Vol "
                vlevel = volume_now.level

                if volume_now.status == "off" then
                    vlevel = vlevel .. "M "
                else
                    vlevel = vlevel .. " "
                end

                widget:set_markup(markup.font(theme.font, markup(gray, header) .. vlevel))
            end,
        })

        -- Add widgets to the wibox
        s.mywibox:setup({
            layout = wibox.layout.align.horizontal,
            { -- Left widgets
                layout = wibox.layout.fixed.horizontal,
                launcher.mylauncher,
                s.mytaglist,
                s.mypromptbox,
            },
            s.mytasklist, -- Middle widget
            { -- Right widgets
                layout = wibox.layout.fixed.horizontal,
                mykeyboardlayout,
                volume.widget,
                -- lain.widget.cpu({
                --     settings = function()
                --         widget:set_markup("Cpu " .. cpu_now.usage)
                --     end,
                -- }).widget,
                wibox.widget.systray(),
                mytextclock,
                s.mylayoutbox,
            },
        })
    end)
end

return M
