local gears = require("gears")
local awful = require("awful")
local vars = require("modules.variables")
local hotkeys_popup = require("awful.hotkeys_popup")

local util = require("util.common-util")
local scratchpads = require("modules.scratchpads")
local func = require("modules.funcs")

local M = {}

M.globalkeys = gears.table.join(
    awful.key({ vars.key.modkey }, "s", hotkeys_popup.show_help, { description = "show help", group = "awesome" }),
    -- awful.key({ modkey }, "Left", awful.tag.viewprev, { description = "view previous", group = "tag" }),
    -- awful.key({ modkey }, "Right", awful.tag.viewnext, { description = "view next", group = "tag" }),
    -- awful.key({ modkey }, "Escape", awful.tag.history.restore, { description = "go back", group = "tag" }),

    -- Focus window
    awful.key({ vars.key.modkey }, "j", function()
        --awful.client.focus.byidx(1)
        awful.client.focus.bydirection("down")
    end, { description = "focus next by index", group = "client" }),
    awful.key({ vars.key.modkey }, "k", function()
        awful.client.focus.bydirection("up")
        --awful.client.focus.byidx(-1)
    end, { description = "focus previous by index", group = "client" }),
    awful.key({ vars.key.modkey }, "h", function()
        awful.client.focus.bydirection("left")
    end, { description = "focus next by index", group = "client" }),
    awful.key({ vars.key.modkey }, "l", function()
        awful.client.focus.bydirection("right")
    end, { description = "focus previous by index", group = "client" }),

    -- Change size
    awful.key({ vars.key.modkey, "Control" }, "l", function()
        awful.tag.incmwfact(0.05)
    end, { description = "increase master width factor", group = "layout" }),
    awful.key({ vars.key.modkey, "Control" }, "h", function()
        awful.tag.incmwfact(-0.05)
    end, { description = "decrease master width factor", group = "layout" }),
    awful.key({ vars.key.modkey, "Control" }, "k", function()
        awful.client.incwfact(0.05)
    end, { description = "increase master width factor", group = "layout" }),
    awful.key({ vars.key.modkey, "Control" }, "j", function()
        awful.client.incwfact(-0.05)
    end, { description = "decrease master width factor", group = "layout" }),

    awful.key({ vars.key.modkey, "Shift" }, "h", function()
        awful.tag.incnmaster(1, nil, true)
    end, { description = "increase the number of master clients", group = "layout" }),
    awful.key({ vars.key.modkey, "Shift" }, "l", function()
        awful.tag.incnmaster(-1, nil, true)
    end, { description = "decrease the number of master clients", group = "layout" }),
    -- awful.key({ modkey, "Control" }, "h", function()
    --     awful.tag.incncol(1, nil, true)
    -- end, { description = "increase the number of columns", group = "layout" }),
    -- awful.key({ modkey, "Control" }, "l", function()
    --     awful.tag.incncol(-1, nil, true)
    -- end, { description = "decrease the number of columns", group = "layout" }),

    -- App Runners
    awful.key({ vars.key.altkey }, "space", function()
        awful.util.spawn(vars.path.mymenu)
    end, { description = "show my applications menu", group = "awesome" }),
    awful.key({ vars.key.modkey }, "Return", function()
        awful.spawn(vars.app.terminal)
    end, { description = "open a terminal", group = "launcher" }),
    awful.key({ vars.key.modkey, "Shift" }, "r", awesome.restart, { description = "reload awesome", group = "awesome" }),
    -- awful.key({ modkey, "Shift" }, "q", awesome.quit, { description = "quit awesome", group = "awesome" }),

    -- Layout manipulation
    awful.key({ vars.key.modkey, "Shift" }, "j", function()
        awful.client.swap.byidx(1)
    end, { description = "swap with next client by index", group = "client" }),
    awful.key({ vars.key.modkey, "Shift" }, "k", function()
        awful.client.swap.byidx(-1)
    end, { description = "swap with previous client by index", group = "client" }),
    awful.key({ vars.key.modkey, "Control" }, "j", function()
        awful.screen.focus_relative(1)
    end, { description = "focus the next screen", group = "screen" }),
    awful.key({ vars.key.modkey, "Control" }, "k", function()
        awful.screen.focus_relative(-1)
    end, { description = "focus the previous screen", group = "screen" }),
    awful.key(
        { vars.key.modkey },
        "u",
        awful.client.urgent.jumpto,
        { description = "jump to urgent client", group = "client" }
    ),
    awful.key({ vars.key.altkey }, "Tab", function()
        awful.client.focus.history.previous()
        if client.focus then
            client.focus:raise()
        end
    end, { description = "go back", group = "client" }),

    -- awful.key({ modkey }, "space", function()
    --     awful.layout.inc(1)
    -- end, { description = "select next", group = "layout" }),
    awful.key({ vars.key.modkey }, "Tab", function()
        awful.layout.inc(-1)
    end, { description = "select previous", group = "layout" }),

    awful.key({ vars.key.modkey, "Control" }, "n", function()
        local c = awful.client.restore()
        -- Focus restored client
        if c then
            c:emit_signal("request::activate", "key.unminimize", { raise = true })
        end
    end, { description = "restore minimized", group = "client" }),

    -- Scratchpads
    awful.key({ vars.key.modkey }, "y", function()
        util.notify("📂 Yazi File Manager")
        scratchpads.yazi:toggle()
    end),
    awful.key({ vars.key.modkey }, "t", function()
        util.notify("💬 Telegram")
        scratchpads.telegram:toggle()
    end),
    awful.key({ vars.key.modkey }, "m", function()
        util.notify("💽 Music")
        scratchpads.youtube_music:toggle()
    end),
    awful.key({ vars.key.modkey }, "g", function()
        util.notify("✉️ Chat")
        scratchpads.google_chat:toggle()
    end),
    awful.key({ vars.key.modkey }, "n", function()
        util.notify("📂 Nautilus")
        scratchpads.nautilus:toggle()
    end),

    awful.key(
        { vars.key.modkey },
        "p",
        func.keychord({
            y = function()
                scratchpads.yazi:toggle()
            end,
            t = function()
                scratchpads.telegram:toggle()
            end,
            m = function()
                scratchpads.youtube_music:toggle()
            end,
            g = function()
                scratchpads.google_chat:toggle()
            end,
        })
    ),

    -- Prompt
    awful.key({ vars.key.modkey }, "r", function()
        awful.screen.focused().mypromptbox:run()
    end, { description = "run prompt", group = "launcher" }),

    awful.key({ vars.key.modkey }, "x", function()
        awful.prompt.run({
            prompt = "Run Lua code: ",
            textbox = awful.screen.focused().mypromptbox.widget,
            exe_callback = awful.util.eval,
            history_path = awful.util.get_cache_dir() .. "/history_eval",
        })
    end, { description = "lua execute prompt", group = "awesome" }),
    -- Menubar
    -- awful.key({ modkey }, "p", function()
    --     menubar.show()
    -- end, { description = "show the menubar", group = "launcher" })

    -- Show/Hide Wibox
    awful.key({ vars.key.modkey }, "b", function()
        for s in screen do
            s.mywibox.visible = not s.mywibox.visible
            if s.mybottomwibox then
                s.mybottomwibox.visible = not s.mybottomwibox.visible
            end
        end
    end, { description = "toggle wibox", group = "awesome" }),

    awful.key({ vars.key.modkey, "Shift" }, "m", function()
        local tag = awful.screen.focused().selected_tag
        if not tag then
            return
        end

        local all_minimized = true
        for _, c in ipairs(tag:clients()) do
            if not c.minimized then
                all_minimized = false
                break
            end
        end

        for _, c in ipairs(tag:clients()) do
            c.minimized = not all_minimized
        end
    end, { description = "toggle hide/show all windows", group = "awesome" })
)

return M
