local awful = require("awful")
local beautiful = require("beautiful")
local tags = require("modules.wibar.taglist-widget").tags
local util = require("util.common-util")

local M = {}
---@param opts { keybind: {} }
M.setup = function(opts)
    -- Table of layouts to cover with awful.layout.inc, order matters.
    --awful.layout.layouts = {
    awful.layout.append_default_layouts({
        awful.layout.suit.tile,
        awful.layout.suit.max,

        -- interesting
        --awful.layout.suit.spiral,
        --awful.layout.suit.spiral.dwindle,
        --awful.layout.suit.magnifier,
        --awful.layout.suit.corner.nw,

        --awful.layout.suit.floating,
        --awful.layout.suit.tile.left,
        --awful.layout.suit.tile.bottom,
        --awful.layout.suit.tile.top,
        --awful.layout.suit.fair,
        --awful.layout.suit.fair.horizontal,

        -- awful.layout.suit.max.fullscreen,
        -- awful.layout.suit.corner.ne,
        -- awful.layout.suit.corner.sw,
        -- awful.layout.suit.corner.se,
    })
    -- }}}

    -- {{{ Rules

    -- Rules to apply to new clients (through the "manage" signal).
    awful.rules.rules = {
        -- All clients will match this rule.
        {
            rule = {},
            properties = {
                border_width = beautiful.border_width,
                border_color = beautiful.border_normal,
                focus = awful.client.focus.filter,
                raise = true,
                keys = opts.keybind.clientkeys,
                buttons = opts.keybind.clientbuttons,
                screen = awful.screen.preferred,
                placement = awful.placement.no_overlap + awful.placement.no_offscreen,
            },
        },

        -- Floating clients.
        {
            rule_any = {
                instance = {
                    "DTA", -- Firefox addon DownThemAll.
                    "copyq", -- Includes session name in class.
                    "pinentry",
                    "htop_info",
                    --"disc_usabe_info",
                },
                class = {
                    "Arandr",
                    "Blueman-manager",
                    "Gpick",
                    "Kruler",
                    "MessageWin", -- kalarm.
                    "Sxiv",
                    "Tor Browser", -- Needs a fixed window size to avoid fingerprinting by screen size.
                    "Wpa_gui",
                    "veromix",
                    "xtightvncviewer",

                    "qBittorrent",
                    "pavucontrol",
                    "gnome-system-monitor",
                    "gnome-control-center",
                    "gnome-calculator",
                    "org.gnome.Characters",
                    "org.gnome.clocks",
                    "gnome-calendar",
                    "Gnome-disks",
                    "Nm-connection-editor",
                    "ViberPC",
                    "vlc",
                    "snapshot",
                    "Gcolor3",
                    "Glate",
                },

                -- Note that the name property shown in xprop might be set slightly after creation of the client
                -- and the name shown there might not match defined rules here.
                name = {
                    "Event Tester", -- xev.
                },
                role = {
                    "AlarmWindow", -- Thunderbird's calendar.
                    "ConfigManager", -- Thunderbird's about:config.
                    "pop-up", -- e.g. Google Chrome's (detached) Developer Tools.
                },
            },
            properties = {
                floating = true,
                width = util.calculate_window_width(),
                height = util.calculate_window_height(),
                placement = awful.placement.centered,
            },
        },
        {
            --rule = { instance = "disc_usage_info" },
            rule = { instance = "disc_ugd" },
            properties = {
                floating = true,
                width = util.calculate_window_width(),
                height = util.calculate_window_height(),
                placement = awful.placement.centered,
            },
        },

        -- Scratchpads
        -- {
        --     rule = { class = "com.scratchpad.yazi" },
        --     -- properties = { opacity = 0.85 }, -- moved to picom
        -- },
        -- {
        --     rule = { class = "TelegramDesktop", instance = "telegram-desktop" },
        --     -- properties = { opacity = 0.9 }, -- moved to picom
        -- },
        -- {
        --     -- music
        --     rule = { class = "Brave-browser", instance = "crx_cinhimbnkkaeohfgghhklpknlkffjgod" },
        --     -- properties = { opacity = 0.9 }, -- moved to picom
        -- },
        -- {
        --     -- chat
        --     rule = { class = "Brave-browser", instance = "crx_mdpkiolbdkhdjpekfbkbmhigcaggjagi" },
        --     -- properties = { opacity = 0.9 }, -- moved to picom
        -- },
        -- {
        --     -- monkeytype
        --     rule = { class = "Brave-browser", instance = "crx_picebhhlijnlefeleilfbanaghjlkkna" },
        --     -- properties = { opacity = 0.95 }, -- moved to picom
        -- },
        -- Apps start on tag roles
        {
            rule = { class = "org.wezfurlong.wezterm" },
            properties = { tag = tags[1] },
        },
        {
            rule = { class = "com.ghostty.group01", instance = "ghostty" },
            properties = { tag = tags[1] },
        },
        {
            rule = { class = "jetbrains-idea" },
            properties = { tag = tags[2] },
        },
        {
            rule = { class = "Code" },
            properties = { tag = tags[2] },
        },
        -- {
        --     rule = { class = "Google-chrome", instance = "google-chrome" },
        --     properties = { tag = tags[3] },
        -- },
        {
            rule = { class = "Brave-browser", instance = "brave-browser" },
            properties = { tag = tags[2] },
        },
        -- {
        --     rule = { instance = "kitty" },
        --     properties = { tag = tags[4] },
        -- },

        -- Add titlebars to normal clients and dialogs
        { rule_any = { type = { "normal", "dialog" } }, properties = { titlebars_enabled = false } },

        -- Set Firefox to always map on the tag named "2" on screen 1.
        -- { rule = { class = "Firefox" },
        --   properties = { screen = 1, tag = "2" } },
    }
    -- }}}
end

return M