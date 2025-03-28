local awful = require("awful")
local beautiful = require("beautiful")
local wibox = require("wibox")
local gears = require("gears")

local M = {}

M.setup = function()
    -- {{{ Signals
    -- Signal function to execute when a new client appears.
    client.connect_signal("manage", function(c)
        -- Set the windows at the slave,
        -- i.e. put it at the end of others instead of setting it master.
        if not awesome.startup then
            awful.client.setslave(c)
        end

        if awesome.startup and not c.size_hints.user_position and not c.size_hints.program_position then
            -- Prevent clients from being unreachable after screen count changes.
            awful.placement.no_offscreen(c)
        end

        -- open any floating app on exact screen center
        if c.floating then
            -- Get the screen geometry
            local screen_geometry = c.screen.workarea

            -- Calculate the position to center the window
            local window_width = c.width
            local window_height = c.height

            local x = screen_geometry.x + (screen_geometry.width - window_width) / 2
            local y = screen_geometry.y + (screen_geometry.height - window_height) / 2

            -- Set the client's position
            c:geometry({ x = x, y = y })
        end
    end)

    -- Add a titlebar if titlebars_enabled is set to true in the rules.
    client.connect_signal("request::titlebars", function(c)
        -- buttons for the titlebar
        local buttons = gears.table.join(
            awful.button({}, 1, function()
                c:emit_signal("request::activate", "titlebar", { raise = true })
                awful.mouse.client.move(c)
            end),
            awful.button({}, 3, function()
                c:emit_signal("request::activate", "titlebar", { raise = true })
                awful.mouse.client.resize(c)
            end)
        )

        awful.titlebar(c):setup({
            { -- Left
                awful.titlebar.widget.iconwidget(c),
                buttons = buttons,
                layout = wibox.layout.fixed.horizontal,
            },
            { -- Middle
                { -- Title
                    align = "center",
                    widget = awful.titlebar.widget.titlewidget(c),
                },
                buttons = buttons,
                layout = wibox.layout.flex.horizontal,
            },
            { -- Right
                awful.titlebar.widget.floatingbutton(c),
                awful.titlebar.widget.maximizedbutton(c),
                awful.titlebar.widget.stickybutton(c),
                awful.titlebar.widget.ontopbutton(c),
                awful.titlebar.widget.closebutton(c),
                layout = wibox.layout.fixed.horizontal(),
            },
            layout = wibox.layout.align.horizontal,
        })
    end)

    -- Enable sloppy focus, so that focus follows mouse.
    client.connect_signal("mouse::enter", function(c)
        c:emit_signal("request::activate", "mouse_enter", { raise = false })
    end)

    client.connect_signal("focus", function(c)
        c.border_color = beautiful.border_focus
    end)
    client.connect_signal("unfocus", function(c)
        c.border_color = beautiful.border_normal
    end)
    -- }}}

    -- local function set_wallpaper(s)
    --     -- Wallpaper
    --     if beautiful.wallpaper then
    --         local wallpaper = beautiful.wallpaper
    --         -- If wallpaper is a function, call it with the screen
    --         if type(wallpaper) == "function" then
    --             wallpaper = wallpaper(s)
    --         end
    --         gears.wallpaper.maximized(wallpaper, s, true)
    --     end
    -- end

    -- Re-set wallpaper when a screen's geometry changes (e.g. different resolution)
    -- screen.connect_signal("property::geometry", set_wallpaper)
end

return M
