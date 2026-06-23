local gamemode = require("utils.gamemode")
local alt_tab = require("utils.alt_tab_global")
local common_util = require("utils.common-util")
local keys = common_util.keys
local c = require("utils.common-vars")
-- local log = require("utils.logs-util").log

-----------------------------------------------------
----------------------- Basics ----------------------
-----------------------------------------------------

-- Kill focused window
hl.bind(keys(c.mainMod, c.shift, "C"), hl.dsp.window.close())
hl.bind(keys(c.mainMod, c.ctrl, "C"), hl.dsp.exec_cmd(c.killmenu))

-- Start your launcher
hl.bind(keys(c.alt, "SPACE"), hl.dsp.exec_cmd(c.menu))
hl.bind(keys(c.mainMod, "Q"), hl.dsp.exec_cmd(c.qmenu))
hl.bind(keys(c.mainMod, "V"), hl.dsp.exec_cmd(c.clipmenu))

-- Other
-- hl.bind(keys(c.ctrl, c.alt, "Q"), hl.dsp.exec_cmd("~/dotfiles/scripts/screen-lockw"))
hl.bind(keys(c.ctrl, c.alt, "Q"), hl.dsp.exec_cmd("hyprlock"))
hl.bind(keys(c.mainMod, "SPACE"), hl.dsp.exec_cmd("~/.config/waybar/module/shell/change_language.sh hyprland"))
hl.bind(keys(c.mainMod, c.shift, "R"), hl.dsp.exec_cmd("hyprctl reload"))

-- Toggle waybar visibility
hl.bind(keys(c.mainMod, "B"), hl.dsp.exec_cmd("killall -SIGUSR1 waybar"))

----------------------------------------------------------
----------------------- Navigation -----------------------
----------------------------------------------------------

-- Move focus with mod + vim keys (Common)
hl.bind(keys(c.mainMod, c.vleft), hl.dsp.focus({ direction = "l" }))
hl.bind(keys(c.mainMod, c.vright), hl.dsp.focus({ direction = "r" }))
-- hl.bind(keys(mod, c.vup), hl.dsp.focus({ direction = "u" }))
-- hl.bind(keys(mod, c.vdown), hl.dsp.focus({ direction = "d" }))

hl.bind(keys(c.alt, "tab"), alt_tab.switch)

-- Monitors
hl.bind(keys(c.mainMod, "Period"), hl.dsp.focus({ monitor = "+1" }))
hl.bind(keys(c.mainMod, "Comma"), hl.dsp.focus({ monitor = "-1" }))

-- Scroll through existing workspaces with mod + scroll
-- hl.bind(keys(mod, "mouse_down"), hl.dsp.focus({ workspace = "e+1" }))
-- hl.bind(keys(mod, "mouse_up"), hl.dsp.focus({ workspace = "e-1" }))

-- Move/resize windows with mod + LMB/RMB and dragging
hl.bind(keys(c.mainMod, "mouse:272"), hl.dsp.window.drag(), { mouse = true })
hl.bind(keys(c.mainMod, "mouse:273"), hl.dsp.window.resize(), { mouse = true })

------------------------------------------------------
----------------------- Layouts ----------------------
------------------------------------------------------

-- hl.bind(keys(c.alt, "S"), hl.dsp.window.float({ action = "toggle" }))
-- hl.bind(keys(mod, c.shift, "P"), hl.dsp.exec_cmd("hyprctl dispatch togglefloating && hyprctl dispatch centerwindow"))
hl.bind(keys(c.mainMod, "P"), hl.dsp.window.pseudo()) -- (floating centered)

require("utils.keybindings.master-layout")
-- require("utils.keybindings.dwindle-layout")
-- require("utils.keybindings.tabbed-layout")

----------------------------------------------------
----------------------- Resize ---------------------
----------------------------------------------------

hl.bind(keys(c.ctrl, c.mainMod, c.vright), hl.dsp.window.resize({ x = 100, y = 0, relative = true }), { repeating = true })
hl.bind(keys(c.ctrl, c.mainMod, c.vleft), hl.dsp.window.resize({ x = -100, y = 0, relative = true }), { repeating = true })
hl.bind(keys(c.ctrl, c.mainMod, c.vup), hl.dsp.window.resize({ x = 0, y = -100, relative = true }), { repeating = true })
hl.bind(keys(c.ctrl, c.mainMod, c.vdown), hl.dsp.window.resize({ x = 0, y = 100, relative = true }), { repeating = true })

----------------------------------------------------
----------------------- Apps -----------------------
----------------------------------------------------

-- Start a terminal
hl.bind(keys(c.mainMod, "RETURN"), hl.dsp.exec_cmd(c.terminal))

-- Open/close gnome-control-center
-- stylua: ignore start
hl.bind(keys(c.alt, c.shift, "G"), hl.dsp.exec_cmd("kill -9 $(pidof gnome-control-center) || XDG_CURRENT_DESKTOP=GNOME gnome-control-center"))
-- stylua: ignore end
-- hl.bind(keys(c.alt, c.shift, "W"), hl.dsp.exec_cmd("~/.config/waypaper/toggle.sh hyprland"))
hl.bind(keys(c.alt, c.shift, "W"), hl.dsp.exec_cmd("foot --app-id com.scratchpad.wallpapers ~/dotfiles/scripts/wallpapers/wallpaper-selector.sh"))
hl.bind(keys(c.alt, c.shift, "B"), hl.dsp.exec_cmd("~/dotfiles/scripts/start-browserw"))

hl.bind("Print", hl.dsp.exec_cmd("~/dotfiles/scripts/screenshot.w.sh"))
hl.bind("Pause", hl.dsp.exec_cmd("~/dotfiles/scripts/screenshot.w.sh"))

----------------------------------------------------
----------------------- Games ----------------------
----------------------------------------------------

hl.bind(keys(c.mainMod, "F1"), gamemode.toggle)

----------------------------------------------------
----------------------- Media ----------------------
----------------------------------------------------

-- Monitor brightness control
hl.bind(keys(c.mainMod, "Page_Up"), hl.dsp.exec_cmd("echo '+' > /tmp/waybar-ddc-module-rx"))
hl.bind(keys(c.mainMod, "Page_Down"), hl.dsp.exec_cmd("echo '-' > /tmp/waybar-ddc-module-rx"))
hl.bind(keys(c.mainMod, c.shift, "Page_Down"), hl.dsp.exec_cmd("echo '=' > /tmp/waybar-ddc-module-rx"))

-- Monitor color temperature control
-- stylua: ignore start
hl.bind(keys(c.mainMod, "Home"), hl.dsp.exec_cmd("~/.config/waybar/module/shell/module.hyprsunset + ; pkill -RTMIN+4 waybar"))
hl.bind(keys(c.mainMod, "End"), hl.dsp.exec_cmd("~/.config/waybar/module/shell/module.hyprsunset - ; pkill -RTMIN+4 waybar"))
hl.bind(keys(c.mainMod, c.shift, "Home"), hl.dsp.exec_cmd("~/.config/waybar/module/shell/module.hyprsunset = ; pkill -RTMIN+4 waybar"))
hl.bind(keys(c.mainMod, c.shift, "End"), hl.dsp.exec_cmd("~/.config/waybar/module/shell/module.hyprsunset bblock ; pkill -RTMIN+4 waybar"))
-- stylua: ignore end

-- sudo ddcutil getvcp 10
-- ddcutil getvcp 10 --display 1 #VCP code 0x10 (Brightness): current value = 40, max value = 100
-- ddcutil getvcp 10 --display 2 #VCP code 0x10 (Brightness): current value = 56, max value = 100

-- Volume control
hl.bind(keys(c.mainMod, "left"), hl.dsp.exec_cmd("wpctl set-volume @DEFAULT_AUDIO_SINK@ 5%-"))
hl.bind(keys(c.mainMod, "right"), hl.dsp.exec_cmd("wpctl set-volume -l 1 @DEFAULT_AUDIO_SINK@ 5%+"))
hl.bind(keys(c.mainMod, "down"), hl.dsp.exec_cmd("wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle"))

-- Player control
hl.bind(keys(c.mainMod, c.shift, "left"), hl.dsp.exec_cmd("playerctl previous"))
hl.bind(keys(c.mainMod, c.shift, "right"), hl.dsp.exec_cmd("playerctl next"))
hl.bind(keys(c.mainMod, c.shift, "down"), hl.dsp.exec_cmd("playerctl play-pause"))

-- MPD control
hl.bind(keys(c.mainMod, c.ctrl, "down"), hl.dsp.exec_cmd("mpc toggle"))
hl.bind(keys(c.mainMod, c.ctrl, "up"), hl.dsp.exec_cmd("mpc stop"))
hl.bind(keys(c.mainMod, c.ctrl, "right"), hl.dsp.exec_cmd("mpc next"))
hl.bind(keys(c.mainMod, c.ctrl, "left"), hl.dsp.exec_cmd("mpc prev"))

----------------------------------------------------
-------------------- Workspace ---------------------
----------------------------------------------------

-- Switch workspaces with mod + [1-9]
-- Move active window to a workspace with mod + c.shift + [1-9]
for i = 1, 9 do
    hl.bind(keys(c.mainMod, tostring(i)), hl.dsp.focus({ workspace = i }))
    hl.bind(keys(c.mainMod, c.shift, tostring(i)), hl.dsp.window.move({ workspace = i }))
end

-- Special workspace (scratchpad)
-- hl.bind(keys(mod, "O"), hl.dsp.workspace.toggle_special("scratchpad"))
-- hl.bind(keys(mod, c.shift, "O"), hl.dsp.window.move({ workspace = "special:scratchpad" }))
hl.bind(keys(c.mainMod, c.shift, "S"), hl.dsp.workspace.toggle_special("scratchpad"))

-- Scroll through existing workspaces with c.mainMod + scroll
hl.bind(keys(c.mainMod, "mouse_down"), hl.dsp.focus({ workspace = "e+1" }))
hl.bind(keys(c.mainMod, "mouse_up"), hl.dsp.focus({ workspace = "e-1" }))

-- Move/resize windows with c.mainMod + LMB/RMB and dragging
hl.bind(keys(c.mainMod, "mouse:272"), hl.dsp.window.drag(), { mouse = true })
hl.bind(keys(c.mainMod, "mouse:273"), hl.dsp.window.resize(), { mouse = true })

----------------------------------------------------
---------------------- Submap ----------------------
----------------------------------------------------

-- Switch to a submap called "resize"
hl.bind(keys(c.alt, "R"), hl.dsp.submap("resize"))

-- Define the "resize" submap
hl.define_submap("resize", function()
    -- Repeatable binds for resizing the active window
    hl.bind("right", hl.dsp.window.resize({ x = 10, y = 0, relative = true }), { repeating = true })
    hl.bind("left", hl.dsp.window.resize({ x = -10, y = 0, relative = true }), { repeating = true })
    hl.bind("up", hl.dsp.window.resize({ x = 0, y = -10, relative = true }), { repeating = true })
    hl.bind("down", hl.dsp.window.resize({ x = 0, y = 10, relative = true }), { repeating = true })

    -- Use escape to go back to the global submap
    hl.bind("escape", hl.dsp.submap("reset"))
end)

----------------------------------------------------
----------------------- Zoom -----------------------
----------------------------------------------------

-- hl.bind(keys(c.mainMod, "equal"), hl.dsp.exec_cmd("hyprctl -q keyword cursor:zoom_factor $(hyprctl getoption cursor:zoom_factor | awk '/^float.*/ {print $2 * 1.1}')"))
-- hl.bind(keys(c.mainMod, "minus"), hl.dsp.exec_cmd("hyprctl -q keyword cursor:zoom_factor $(hyprctl getoption cursor:zoom_factor | awk '/^float.*/ {print $2 * 0.9}')"))
-- hl.bind(keys(c.mainMod, c.shift, "minus"), hl.dsp.exec_cmd("hyprctl -q keyword cursor:zoom_factor 1"))
