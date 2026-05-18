------------------------------------------------------------
------------------- Scratchpad bindings --------------------
------------------------------------------------------------

-- Uses variables from keybindings.lua
local mod = "SUPER"
local alt = "ALT"
local shift = "SHIFT"

hl.bind(mod .. " + T", hl.dsp.exec_cmd("~/.config/hypr/scripts/toggle_scratchpad_app.sh telegram"))
hl.bind(mod .. " + " .. shift .. " + T", hl.dsp.exec_cmd("~/.config/hypr/scripts/toggle_scratchpad_app.sh mini_terminal"))
hl.bind(mod .. " + Y", hl.dsp.exec_cmd("~/.config/hypr/scripts/toggle_scratchpad_app.sh yazi"))
hl.bind(mod .. " + N", hl.dsp.exec_cmd("~/.config/hypr/scripts/toggle_scratchpad_app.sh youtube_music"))
hl.bind(mod .. " + M", hl.dsp.exec_cmd("~/.config/hypr/scripts/toggle_scratchpad_app.sh music"))
hl.bind(mod .. " + G", hl.dsp.exec_cmd("~/.config/hypr/scripts/toggle_scratchpad_app.sh google_chat"))
hl.bind(mod .. " + U", hl.dsp.exec_cmd("~/.config/hypr/scripts/toggle_scratchpad_app.sh monkey_type"))
hl.bind(mod .. " + E", hl.dsp.exec_cmd("~/.config/hypr/scripts/toggle_scratchpad_app.sh nautilus"))
hl.bind(alt .. " + " .. shift .. " + V", hl.dsp.exec_cmd("~/.config/hypr/scripts/toggle_scratchpad_app.sh vim_hero"))

------------------------------------------------------------
--------------------- Scratchpad rules ---------------------
------------------------------------------------------------

local scratchpad_apps = "("
    .. "com.scratchpad.yazi|"
    .. "com.scratchpad.music|"
    .. "org.telegram.desktop|"
    .. "org.gnome.Nautilus|"
    .. "brave-cinhimbnkkaeohfgghhklpknlkffjgod-Default|"
    .. "brave-mdpkiolbdkhdjpekfbkbmhigcaggjagi-Default|"
    .. "brave-picebhhlijnlefeleilfbanaghjlkkna-Default|"
    .. "brave-beifkklpdmlhanbkafbcldldbgnglbpn-Default"
    .. ")"

-- Scratchpads (to apply float as soon as appear, also for when app running not as scratchpad)
hl.window_rule({
    name = "scratchpad_rule",
    match = { class = scratchpad_apps },
    tag = "+scratchpad",
    -- opacity = "0.97",
})

-------------- special

-- Open telegram media viewer on current workspace (where toggled telegram) instead of scratchpad ws
hl.window_rule({
    name = "telegram_viewer",
    match = {
        class = "(org.telegram.desktop)",
        title = "(Media viewer)",
    },
    opacity = "0.93",
    workspace = "current",
    float = true,
    size = "(monitor_w*0.99) (monitor_h*0.97)",
    center = true,
})
