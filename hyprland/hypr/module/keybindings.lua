local gamemode = require("utils.gamemode")
local keys = require("utils.common-util").keys

--------------------------------------------------------
----------------------- Variables ----------------------
--------------------------------------------------------
-- See https://wiki.hypr.land/Configuring/Start/

-- Set programs that you use
-- local terminal = "ghostty"
-- local terminal = "alacritty"
local terminal = "foot"
local fileManager = "nautilus"
local menu = "~/.config/rofi/scripts/launcher_t1"
local qmenu = "~/.config/rofi/scripts/powermenu_t1"
local clipmenu = "~/.config/rofi/cliphist/cliphist-menu"
local killmenu = "~/.config/rofi/killmenu/kill-menu"

-- Modifier keys
local mainMod = "SUPER"
local alt = "ALT"
local shift = "SHIFT"
local ctrl = "CTRL"

-- Vim-style direction keys
local vleft = "h"
local vdown = "j"
local vup = "k"
local vright = "l"

-----------------------------------------------------
----------------------- Basics ----------------------
-----------------------------------------------------

-- Kill focused window
hl.bind(keys(mainMod, shift, "C"), hl.dsp.window.close())
hl.bind(keys(mainMod, ctrl, "C"), hl.dsp.exec_cmd(killmenu))

-- Start your launcher
hl.bind(keys(alt, "SPACE"), hl.dsp.exec_cmd(menu))
hl.bind(keys(mainMod, "Q"), hl.dsp.exec_cmd(qmenu))
hl.bind(keys(mainMod, "V"), hl.dsp.exec_cmd(clipmenu))

-- Other
-- hl.bind(keys(ctrl, alt, "Q"), hl.dsp.exec_cmd("~/dotfiles/bin/screen-lockw"))
hl.bind(keys(ctrl, alt, "Q"), hl.dsp.exec_cmd("hyprlock"))
hl.bind(keys(mainMod, "SPACE"), hl.dsp.exec_cmd("~/.config/waybar/module/shell/change_language.sh hyprland"))
hl.bind(keys(mainMod, shift, "R"), hl.dsp.exec_cmd("hyprctl reload"))

-- Toggle waybar visibility
hl.bind(keys(mainMod, "B"), hl.dsp.exec_cmd("killall -SIGUSR1 waybar"))

----------------------------------------------------------
----------------------- Navigation -----------------------
----------------------------------------------------------

-- Move focus with mod + vim keys
hl.bind(keys(mainMod, vleft), hl.dsp.focus({ direction = "l" }))
hl.bind(keys(mainMod, vright), hl.dsp.focus({ direction = "r" }))
-- hl.bind(keys(mod, vup), hl.dsp.focus({ direction = "u" }))
-- hl.bind(keys(mod, vdown), hl.dsp.focus({ direction = "d" }))
hl.bind(keys(mainMod, vup), hl.dsp.window.cycle_next({ next = false }))
hl.bind(keys(mainMod, vdown), hl.dsp.window.cycle_next())

-- hl.bind(keys(alt, "Tab"), hl.dsp.exec_cmd("..."))
local alt_tab = require("utils.alt_tab_global")
hl.bind(keys(alt, "tab"), function()
    alt_tab.switch()
end)

--------------------------------------

hl.bind(keys(mainMod, shift, vleft), hl.dsp.window.move({ direction = "l" }))
hl.bind(keys(mainMod, shift, vright), hl.dsp.window.move({ direction = "r" }))
-- hl.bind(keys(mod, shift, vup), hl.dsp.window.move({ direction = "u" }))
-- hl.bind(keys(mod, shift, vdown), hl.dsp.window.move({ direction = "d" }))
hl.bind(keys(mainMod, shift, vup), hl.dsp.window.swap({ prev = true }))
hl.bind(keys(mainMod, shift, vdown), hl.dsp.window.swap({ next = true }))

-- Monitors
hl.bind(keys(mainMod, "Period"), hl.dsp.focus({ monitor = "+1" }))
hl.bind(keys(mainMod, "Comma"), hl.dsp.focus({ monitor = "-1" }))

-- Scroll through existing workspaces with mod + scroll
-- hl.bind(keys(mod, "mouse_down"), hl.dsp.focus({ workspace = "e+1" }))
-- hl.bind(keys(mod, "mouse_up"), hl.dsp.focus({ workspace = "e-1" }))

-- Move/resize windows with mod + LMB/RMB and dragging
hl.bind(keys(mainMod, "mouse:272"), hl.dsp.window.drag(), { mouse = true })
hl.bind(keys(mainMod, "mouse:273"), hl.dsp.window.resize(), { mouse = true })

----------------------------------------------------
----------------------- Resize ---------------------
----------------------------------------------------

hl.bind(keys(ctrl, mainMod, vright), hl.dsp.window.resize({ x = 100, y = 0, relative = true }), { repeating = true })
hl.bind(keys(ctrl, mainMod, vleft), hl.dsp.window.resize({ x = -100, y = 0, relative = true }), { repeating = true })
hl.bind(keys(ctrl, mainMod, vup), hl.dsp.window.resize({ x = 0, y = -100, relative = true }), { repeating = true })
hl.bind(keys(ctrl, mainMod, vdown), hl.dsp.window.resize({ x = 0, y = 100, relative = true }), { repeating = true })

----------------------------------------------------
----------------------- Apps -----------------------
----------------------------------------------------

-- Start a terminal
hl.bind(keys(mainMod, "RETURN"), hl.dsp.exec_cmd(terminal))

-- Open/close gnome-control-center
-- stylua: ignore start
hl.bind(keys(alt, shift, "G"), hl.dsp.exec_cmd("kill -9 $(pidof gnome-control-center) || XDG_CURRENT_DESKTOP=GNOME gnome-control-center"))
-- stylua: ignore end
hl.bind(keys(alt, shift, "W"), hl.dsp.exec_cmd("~/.config/waypaper/toggle.sh hyprland"))
hl.bind(keys(alt, shift, "B"), hl.dsp.exec_cmd("~/dotfiles/bin/start-browserw"))

hl.bind("Print", hl.dsp.exec_cmd("~/dotfiles/bin/screenshot.w.sh"))
hl.bind("Pause", hl.dsp.exec_cmd("~/dotfiles/bin/screenshot.w.sh"))

------------------------------------------------------
----------------------- Layouts ----------------------
------------------------------------------------------

-- hl.bind(keys(alt, "S"), hl.dsp.window.float({ action = "toggle" }))
-- hl.bind(keys(mod, shift, "P"), hl.dsp.exec_cmd("hyprctl dispatch togglefloating && hyprctl dispatch centerwindow"))
hl.bind(keys(mainMod, "P"), hl.dsp.window.pseudo()) -- dwindle
hl.bind(keys(mainMod, "S"), hl.dsp.layout("togglesplit")) -- dwindle
hl.bind(keys(alt, "S"), hl.dsp.layout("swapsplit"))

-- hl.bind(keys(mod, "F"), hl.dsp.window.fullscreen({ mode = "maximized" })) -- monocle
hl.bind(keys(mainMod, "F"), hl.dsp.group.toggle()) -- tabbed
hl.bind(keys(mainMod, "code:49"), hl.dsp.focus({ last = true })) -- focuscurrentorlast
hl.bind(keys(mainMod, "code:59"), hl.dsp.group.prev()) -- b - back
hl.bind(keys(mainMod, "code:60"), hl.dsp.group.next()) -- f - forward
hl.bind(keys(mainMod, shift, "code:59"), hl.dsp.window.move({ into_group = "l" })) -- Moves the active window into a group left
hl.bind(keys(mainMod, shift, "code:60"), hl.dsp.window.move({ into_group = "r" })) -- Moves the active window into a group right

hl.bind(keys(mainMod, shift, "F"), hl.dsp.window.fullscreen({ mode = "fullscreen" }))

-- hl.bind(keys(mod, "Tab"), hl.dsp.exec_cmd("~/.config/hypr/shell/toggle_layout.sh"))
hl.bind(keys(mainMod, "Tab"), hl.dsp.window.fullscreen({ mode = "maximized" }))

----------------------------------------------------
----------------------- Games ----------------------
----------------------------------------------------

-- stylua: ignore start
hl.bind(keys(mainMod, "F1"), function() gamemode.toggle() end)
-- stylua: ignore end

----------------------------------------------------
----------------------- Media ----------------------
----------------------------------------------------

-- Monitor brightness control
hl.bind(keys(mainMod, "Page_Up"), hl.dsp.exec_cmd("echo '+' > /tmp/waybar-ddc-module-rx"))
hl.bind(keys(mainMod, "Page_Down"), hl.dsp.exec_cmd("echo '-' > /tmp/waybar-ddc-module-rx"))
hl.bind(keys(mainMod, shift, "Page_Down"), hl.dsp.exec_cmd("echo '=' > /tmp/waybar-ddc-module-rx"))

-- Monitor color temperature control
-- stylua: ignore start
hl.bind(keys(mainMod, "Home"), hl.dsp.exec_cmd("~/.config/waybar/module/shell/module.hyprsunset + ; pkill -RTMIN+4 waybar"))
hl.bind(keys(mainMod, "End"), hl.dsp.exec_cmd("~/.config/waybar/module/shell/module.hyprsunset - ; pkill -RTMIN+4 waybar"))
hl.bind(keys(mainMod, shift, "Home"), hl.dsp.exec_cmd("~/.config/waybar/module/shell/module.hyprsunset = ; pkill -RTMIN+4 waybar"))
hl.bind(keys(mainMod, shift, "End"), hl.dsp.exec_cmd("~/.config/waybar/module/shell/module.hyprsunset bblock ; pkill -RTMIN+4 waybar"))
-- stylua: ignore end

-- sudo ddcutil getvcp 10
-- ddcutil getvcp 10 --display 1 #VCP code 0x10 (Brightness): current value = 40, max value = 100
-- ddcutil getvcp 10 --display 2 #VCP code 0x10 (Brightness): current value = 56, max value = 100

-- Volume control
hl.bind(keys(mainMod, "left"), hl.dsp.exec_cmd("wpctl set-volume @DEFAULT_AUDIO_SINK@ 5%-"))
hl.bind(keys(mainMod, "right"), hl.dsp.exec_cmd("wpctl set-volume -l 1 @DEFAULT_AUDIO_SINK@ 5%+"))
hl.bind(keys(mainMod, "down"), hl.dsp.exec_cmd("wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle"))

-- Player control
hl.bind(keys(mainMod, shift, "left"), hl.dsp.exec_cmd("playerctl previous"))
hl.bind(keys(mainMod, shift, "right"), hl.dsp.exec_cmd("playerctl next"))
hl.bind(keys(mainMod, shift, "down"), hl.dsp.exec_cmd("playerctl play-pause"))

-- MPD control
hl.bind(keys(mainMod, ctrl, "down"), hl.dsp.exec_cmd("mpc toggle"))
hl.bind(keys(mainMod, ctrl, "up"), hl.dsp.exec_cmd("mpc stop"))
hl.bind(keys(mainMod, ctrl, "right"), hl.dsp.exec_cmd("mpc next"))
hl.bind(keys(mainMod, ctrl, "left"), hl.dsp.exec_cmd("mpc prev"))

----------------------------------------------------
-------------------- Workspace ---------------------
----------------------------------------------------

-- Switch workspaces with mod + [1-9]
-- Move active window to a workspace with mod + SHIFT + [1-9]
for i = 1, 9 do
    hl.bind(keys(mainMod, tostring(i)), hl.dsp.focus({ workspace = i }))
    hl.bind(keys(mainMod, shift, tostring(i)), hl.dsp.window.move({ workspace = i }))
end

-- Special workspace (scratchpad)
-- hl.bind(keys(mod, "O"), hl.dsp.workspace.toggle_special("scratchpad"))
-- hl.bind(keys(mod, shift, "O"), hl.dsp.window.move({ workspace = "special:scratchpad" }))
hl.bind(keys(mainMod, shift, "S"), hl.dsp.workspace.toggle_special("scratchpad"))

-- Scroll through existing workspaces with mainMod + scroll
hl.bind(keys(mainMod, "mouse_down"), hl.dsp.focus({ workspace = "e+1" }))
hl.bind(keys(mainMod, "mouse_up"), hl.dsp.focus({ workspace = "e-1" }))

-- Move/resize windows with mainMod + LMB/RMB and dragging
hl.bind(keys(mainMod, "mouse:272"), hl.dsp.window.drag(), { mouse = true })
hl.bind(keys(mainMod, "mouse:273"), hl.dsp.window.resize(), { mouse = true })

----------------------------------------------------
---------------------- Submap ----------------------
----------------------------------------------------

-- Switch to a submap called "resize"
hl.bind(keys(alt, "R"), hl.dsp.submap("resize"))

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

-- hl.bind(keys(mainMod, "equal"), hl.dsp.exec_cmd("hyprctl -q keyword cursor:zoom_factor $(hyprctl getoption cursor:zoom_factor | awk '/^float.*/ {print $2 * 1.1}')"))
-- hl.bind(keys(mainMod, "minus"), hl.dsp.exec_cmd("hyprctl -q keyword cursor:zoom_factor $(hyprctl getoption cursor:zoom_factor | awk '/^float.*/ {print $2 * 0.9}')"))
-- hl.bind(keys(mainMod, shift, "minus"), hl.dsp.exec_cmd("hyprctl -q keyword cursor:zoom_factor 1"))