local M = {}

------------------------------------------------------------
----------------------- App Definitions --------------------
------------------------------------------------------------

M.apps = {
    yazi = {
        class = "com.scratchpad.yazi",
        cmd = "foot --app-id=com.scratchpad.yazi -e ~/.cargo/bin/yazi",
        notify = "📂 Yazi Manager",
    },
    mini_terminal = {
        class = "com.scratchpad.mini_terminal",
        cmd = "foot --app-id=com.scratchpad.mini_terminal",
        notify = "🧑🏻‍💻 Mini Terminal",
    },
    music = {
        class = "com.scratchpad.music",
        cmd = "foot --app-id=com.scratchpad.music -e ~/.cargo/bin/rmpc",
        notify = "🎹 MPD-RMPC",
    },
    wallpapers = {
        class = "com.scratchpad.wallpapers",
        cmd = "foot --app-id=com.scratchpad.wallpapers -e ~/dotfiles/scripts/wallpapers/wallpaper-selector.sh",
        notify = "🌆 Wallpapers",
    },
    nautilus = {
        class = "org.gnome.Nautilus",
        cmd = "nautilus",
        notify = "📂 Nautilus",
    },
    telegram = {
        class = "org.telegram.desktop",
        cmd = "Telegram || telegram-desktop || telegram",
        notify = "💬 Telegram",
    },
    youtube_music = {
        class = "brave-cinhimbnkkaeohfgghhklpknlkffjgod-Default",
        cmd = "brave --profile-directory=Default --app-id=cinhimbnkkaeohfgghhklpknlkffjgod",
        notify = "💽 Music",
    },
    google_chat = {
        class = "brave-mdpkiolbdkhdjpekfbkbmhigcaggjagi-Default",
        cmd = "brave --profile-directory=Default --app-id=mdpkiolbdkhdjpekfbkbmhigcaggjagi",
        notify = "✉️ Google Chat",
    },
    monkey_type = {
        class = "brave-picebhhlijnlefeleilfbanaghjlkkna-Default",
        cmd = "brave --profile-directory=Default --app-id=picebhhlijnlefeleilfbanaghjlkkna",
        notify = "  Monkey Type",
    },
    vim_hero = {
        class = "brave-beifkklpdmlhanbkafbcldldbgnglbpn-Default",
        cmd = "brave --profile-directory=Default --app-id=beifkklpdmlhanbkafbcldldbgnglbpn",
        notify = "⌨️ Vim Hero",
    },
}

------------------------------------------------------------
------------------------- Helpers --------------------------
------------------------------------------------------------

local function find_window_by_class(class)
    local windows = hl.get_windows()
    for _, w in ipairs(windows) do
        if w.class == class then
            return w
        end
    end
    return nil
end

local function is_in_scratchpad(window)
    return window.workspace.name == "special:scratchpad"
end

local function set_scratch_roles(window)
    local mon = hl.get_active_monitor()
    local mon_w = math.floor(mon.width / mon.scale)
    local mon_h = math.floor(mon.height / mon.scale)
    local win_w = math.floor(mon_w * 0.75)
    local win_h = math.floor(mon_h * 0.80)
    local addr = "address:" .. window.address

    hl.dispatch(hl.dsp.window.float({ action = "on", window = addr }))
    hl.dispatch(hl.dsp.window.resize({ x = win_w, y = win_h, window = addr }))
    hl.dispatch(hl.dsp.window.center(addr))
    hl.dispatch(hl.dsp.window.set_prop({ prop = "active_border_color", value = "rgb(AB9DF2)", window = addr }))
    hl.dispatch(hl.dsp.window.set_prop({ prop = "border_size", value = "5", window = addr }))
    hl.dispatch(hl.dsp.window.set_prop({ prop = "dim_around", value = "1", window = addr }))
    hl.dispatch(hl.dsp.window.set_prop({ prop = "opacity", value = "0.97", window = addr }))
end

local function hide_other_scratchpads(current_class)
    local windows = hl.get_windows()
    for _, w in ipairs(windows) do
        if w.class ~= current_class and not is_in_scratchpad(w) then
            local has_tag = false
            if w.tags then
                for _, tag in ipairs(w.tags) do
                    if tag:match("^scratchpad") then
                        has_tag = true
                        break
                    end
                end
            end
            if has_tag then
                hl.dispatch(hl.dsp.window.move({ workspace = "special:scratchpad", window = "address:" .. w.address, follow = false }))
            end
        end
    end
end

local function move_to_scratchpad(window)
    hl.dispatch(hl.dsp.window.move({ workspace = "special:scratchpad", window = "address:" .. window.address, follow = false }))
end

local function show_window(window)
    local ws = hl.get_active_workspace()
    local addr = "address:" .. window.address
    hl.dispatch(hl.dsp.window.move({ workspace = ws.name, window = addr, follow = false }))
    hl.dispatch(hl.dsp.focus({ window = addr }))
    set_scratch_roles(window)
end

------------------------------------------------------------
----------------------- Public API -------------------------
------------------------------------------------------------

function M.toggle(app_name)
    local app = M.apps[app_name]
    if not app then
        hl.notification.create({ text = "❌ " .. app_name .. " is unsupported", timeout = 3000, icon = "error" })
        return
    end

    local window = find_window_by_class(app.class)

    if window then
        if is_in_scratchpad(window) then
            hide_other_scratchpads(app.class)
            show_window(window)
            hl.notification.create({ text = app.notify, timeout = 700, icon = "ok" })
        else
            move_to_scratchpad(window)
        end
    else
        -- Launch app and apply roles when window appears
        hl.exec_cmd(app.cmd)
        hl.notification.create({ text = app.notify, timeout = 700, icon = "ok" })
        hl.on("window.open", function(w)
            if w.class == app.class then
                set_scratch_roles(w)
                hide_other_scratchpads(app.class)
                return true -- unregister handler
            end
        end)
    end

    -- signal waybar scratchpad module
    hl.exec_cmd("pkill -RTMIN+3 waybar")
end

return M
