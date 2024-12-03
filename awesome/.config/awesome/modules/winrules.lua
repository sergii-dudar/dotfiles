local awful = require("awful")
local beautiful = require("beautiful")
local keybing = require("modules.keybind")

local M = {}

M.setup = function()
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

    -- beautiful.gap_single_client = true
    -- beautiful.useless_gap = 5

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
                keys = keybing.clientkeys,
                buttons = keybing.clientbuttons,
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
            properties = { floating = true },
        },

        -- Scratchpads
        {
            rule = { class = "yazi" },
            properties = { opacity = 0.85 },
        },
        {
            rule = { class = "telegram-desktop" },
            properties = { opacity = 0.9 },
        },
        {
            -- music
            rule = { class = "Google-chrome", instance = "crx_cinhimbnkkaeohfgghhklpknlkffjgod" },
            properties = { opacity = 0.9 },
        },
        {
            -- chat
            rule = { class = "Google-chrome", instance = "crx_mdpkiolbdkhdjpekfbkbmhigcaggjagi" },
            properties = { opacity = 0.9 },
        },

        -- Apps start on tag roles
        {
            rule = { class = "org.wezfurlong.wezterm" },
            properties = { tag = "1" },
        },
        {
            rule = { class = "jetbrains-idea" },
            properties = { tag = "2" },
        },
        {
            rule = { class = "Code" },
            properties = { tag = "2" },
        },
        -- {
        --     rule = { class = "Google-chrome", instance = "google-chrome" },
        --     properties = { tag = "3" },
        -- },
        {
            rule = { class = "kitty" },
            properties = { tag = "4" },
        },

        -- Add titlebars to normal clients and dialogs
        { rule_any = { type = { "normal", "dialog" } }, properties = { titlebars_enabled = false } },

        -- Set Firefox to always map on the tag named "2" on screen 1.
        -- { rule = { class = "Firefox" },
        --   properties = { screen = 1, tag = "2" } },
    }
    -- }}}
end

return M
