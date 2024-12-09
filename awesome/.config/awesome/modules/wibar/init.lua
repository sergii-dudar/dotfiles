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
local keyboard_layout_widget = require("modules.wibar.keyboard-layout-widget")
local cpu_widget = require("modules.wibar.cpu-widget")
local mpris_widget = require("modules.wibar.mpris-widget")
--local mpris_widget = require("awesome-wm-widgets.mpris-widget")
local vars = require("modules.variables")

local M = {}

---@param opts { keybind: {} }
M.setup = function(opts)
    -- {{{ Wibar

    awful.screen.connect_for_each_screen(function(s)
        --local volume_widget = require("awesome-wm-widgets.volume-widget.volume")
        --sudo pacman -S acpi
        --local batteryarc_widget = require("awesome-wm-widgets.batteryarc-widget.batteryarc")
        --local battery_widget = require("awesome-wm-widgets.battery-widget.battery")

        local gray = "#94928F"
        local markup = lain.util.markup
        local mem = lain.widget.mem({
            settings = function()
                local perc = string.format("%2d", mem_now.perc)
                widget:set_markup(
                    markup.font(
                        vars.font.widget,
                        markup(gray, "<span foreground='#a6e3a1'>  </span>") .. perc .. "%"
                    )
                )
            end,
        })

        local cpu = lain.widget.cpu({
            settings = function()
                local perc = string.format("%2d", cpu_now.usage)
                widget:set_markup(
                    markup.font(
                        vars.font.widget,
                        markup(gray, "<span foreground='#8caaee'>  </span>") .. perc .. "%"
                    )
                )
            end,
        })

        local fs = lain.widget.fs({
            settings = function()
                local perc = string.format("%2d", fs_now["/home"].percentage)
                widget:set_markup(
                    markup.font(
                        vars.font.widget,
                        markup(gray, "<span foreground='#e5c890'>  </span>")
                            .. perc
                            .. "% <span foreground='#e5c890'>SSD</span>"
                    )
                )
            end,
        })

        -- local pulse = lain.widget.pulse({
        --     settings = function()
        --         --local perc = string.format("%2d", volume_now.index)
        --         local perc = volume_now.index
        --         widget:set_markup(
        --             markup.font(vars.font.widget, markup(gray, "<span foreground='#e5c890'>  </span>") .. perc)
        --         )
        --     end,
        -- })

        -- ALSA volume
        local alsa = lain.widget.alsa({
            settings = function()
                local icon = "<span foreground='#ca9ee6'>  </span>"
                if volume_now.status == "off" then
                    icon = "<span foreground='#d35f5e'>  </span>"
                elseif tonumber(volume_now.level) == 0 then
                    icon = "<span foreground='#ca9ee6'>  </span>"
                elseif tonumber(volume_now.level) <= 10 then
                    icon = "<span foreground='#ca9ee6'>  </span>"
                end

                --widget:set_markup(markup.font(theme.font, " " .. volume_now.level .. "% "))
                widget:set_markup(markup.font(vars.font.widget, markup(gray, icon .. volume_now.level .. "%")))
            end,
        })
        alsa.widget:buttons(awful.util.table.join(
            awful.button({}, 1, function()
                os.execute(string.format("%s set %s toggle", alsa.cmd, alsa.togglechannel or alsa.channel))
                alsa.update()
            end),
            awful.button({}, 4, function()
                awful.util.spawn("amixer set Master 1%+")
                alsa.update()
            end),
            awful.button({}, 5, function()
                awful.util.spawn("amixer set Master 1%-")
                alsa.update()
            end)
        ))

        local mpris = awful.widget.watch(
            { awful.util.shell, "-c", "playerctl status && playerctl metadata" },
            2,
            function(widget, stdout)
                local escape_f = require("awful.util").escape
                local mpris_now = {
                    state = "N/A",
                    artist = "N/A",
                    title = "N/A",
                    art_url = "N/A",
                    album = "N/A",
                    album_artist = "N/A",
                }

                mpris_now.state = string.match(stdout, "Playing") or string.match(stdout, "Paused") or "N/A"

                -- for k, v in string.gmatch(stdout, "'[^:]+:([^']+)':[%s]<%[?'([^']+)'%]?>") do
                --     if k == "artUrl" then
                --         mpris_now.art_url = v
                --     elseif k == "artist" then
                --         mpris_now.artist = escape_f(v)
                --     elseif k == "title" then
                --         mpris_now.title = escape_f(v)
                --     elseif k == "album" then
                --         mpris_now.album = escape_f(v)
                --     elseif k == "albumArtist" then
                --         mpris_now.album_artist = escape_f(v)
                --     end
                -- end

                for line in stdout:gmatch("[^\r\n]+") do
                    local key, value = line:match("xesam:(%w+)%s+(.+)")
                    if key and value then
                        if key == "artist" then
                            mpris_now.artist = escape_f(value)
                        elseif key == "title" then
                            mpris_now.title = escape_f(value)
                        elseif key == "album" then
                            mpris_now.album = escape_f(value)
                        elseif key == "albumArtist" then
                            mpris_now.album_artist = escape_f(value)
                        end
                    elseif line:match("mpris:artUrl%s+(.+)") then
                        mpris_now.art_url = line:match("mpris:artUrl%s+(.+)")
                    end
                end
                -- customize here
                --widget:set_text(mpris_now.artist .. " - " .. mpris_now.title)

                --local full_text = mpris_now.artist .. " - " .. mpris_now.title
                local full_text = mpris_now.title
                if #full_text > 30 then
                    full_text = string.sub(full_text, 1, 30) .. "…" -- Add ellipsis for trimmed text
                end

                -- Set font and color
                local font = vars.font.widget
                local color = "#ca9ee6"
                local formatted_text = string.format(" <span font='%s' foreground='#8caaee'> </span>", font)
                    .. string.format("<span font='%s' foreground='%s'>%s</span>", font, color, full_text)

                widget:set_markup(formatted_text)
            end
        )

        local bat = lain.widget.bat({
            settings = function()
                local index, perc = "bat", tonumber(bat_now.perc) or 0

                if perc <= 7 then
                    index = index .. "000"
                elseif perc <= 20 then
                    index = index .. "020"
                elseif perc <= 40 then
                    index = index .. "040"
                elseif perc <= 60 then
                    index = index .. "060"
                elseif perc <= 80 then
                    index = index .. "080"
                elseif perc <= 100 then
                    index = index .. "100"
                end

                if bat_now.ac_status == 1 then
                    index = index .. "charging"
                end

                -- baticon:set_image(theme[index])
                -- battooltip:set_markup(string.format("\n%s%%, %s", perc, bat_now.time))
            end,
        })

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
                        --simple_widget.separator,
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

                            mpris,
                            simple_widget.separator,
                            keyboard_layout_widget.setup(),
                            simple_widget.separator,
                            --pulse,
                            alsa,
                            simple_widget.separator,
                            mem,
                            simple_widget.separator,
                            cpu,
                            simple_widget.separator,
                            fs,

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
