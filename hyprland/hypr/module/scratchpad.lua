local scratchpad = require("utils.toggle_scratchpad_app")
local keys = require("utils.common-util").keys

------------------------------------------------------------
------------------- Scratchpad bindings --------------------
------------------------------------------------------------

local mod = "SUPER"
local alt = "ALT"
local shift = "SHIFT"

-- stylua: ignore start
hl.bind(keys(mod, "T"), function() scratchpad.toggle("telegram") end)
hl.bind(keys(mod, shift, "T"), function() scratchpad.toggle("mini_terminal") end)
hl.bind(keys(mod, "Y"), function() scratchpad.toggle("yazi") end)
hl.bind(keys(mod, "N"), function() scratchpad.toggle("youtube_music") end)
hl.bind(keys(mod, "W"), function() scratchpad.toggle("wallpapers") end)
hl.bind(keys(mod, "M"), function() scratchpad.toggle("music") end)
hl.bind(keys(mod, "G"), function() scratchpad.toggle("google_chat") end)
hl.bind(keys(mod, "U"), function() scratchpad.toggle("monkey_type") end)
hl.bind(keys(mod, "E"), function() scratchpad.toggle("nautilus") end)
hl.bind(keys(alt, shift, "V"), function() scratchpad.toggle("vim_hero") end)
-- stylua: ignore end

------------------------------------------------------------
--------------------- Scratchpad Rules ---------------------
------------------------------------------------------------

local scratchpad_class_regex = "("
    .. "com.scratchpad.yazi|"
    .. "com.scratchpad.music|"
    .. "org.telegram.desktop|"
    .. "org.gnome.Nautilus|"
    .. "brave-cinhimbnkkaeohfgghhklpknlkffjgod-Default|"
    .. "brave-mdpkiolbdkhdjpekfbkbmhigcaggjagi-Default|"
    .. "brave-picebhhlijnlefeleilfbanaghjlkkna-Default|"
    .. "brave-beifkklpdmlhanbkafbcldldbgnglbpn-Default"
    .. ")"

-- Tag scratchpad windows for identification
hl.window_rule({
    name = "scratchpad_rule",
    match = { class = scratchpad_class_regex },
    tag = "+scratchpad",
    -- opacity = 0.97
})

-------------- special

-- Open telegram media viewer on current workspace instead of scratchpad ws
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