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
local mod = "SUPER"
local alt = "ALT"
local shift = "SHIFT"
local ctrl = "CTRL"

-- Vim-style direction keys
local vleft = "h"
local vdown = "j"
local vup = "k"
local vright = "l"

local function keys(...)
    return table.concat({ ... }, " + ")
end

-----------------------------------------------------
----------------------- Basics ----------------------
-----------------------------------------------------

-- Kill focused window
hl.bind(keys(mod, shift, "C"), hl.dsp.window.close())
hl.bind(keys(mod, ctrl, "C"), hl.dsp.exec_cmd(killmenu))

-- Start your launcher
hl.bind(keys(alt, "SPACE"), hl.dsp.exec_cmd(menu))
hl.bind(keys(mod, "Q"), hl.dsp.exec_cmd(qmenu))
hl.bind(keys(mod, "V"), hl.dsp.exec_cmd(clipmenu))

-- Other
-- hl.bind(keys(ctrl, alt, "Q"), hl.dsp.exec_cmd("~/dotfiles/bin/screen-lockw"))
hl.bind(keys(ctrl, alt, "Q"), hl.dsp.exec_cmd("hyprlock"))
hl.bind(keys(mod, "SPACE"), hl.dsp.exec_cmd("~/.config/waybar/module/shell/change_language.sh hyprland"))
hl.bind(keys(mod, shift, "R"), hl.dsp.exec_cmd("hyprctl reload"))

-- Toggle waybar visibility
hl.bind(keys(mod, "B"), hl.dsp.exec_cmd("killall -SIGUSR1 waybar"))

----------------------------------------------------------
----------------------- Navigation -----------------------
----------------------------------------------------------

-- Move focus with mod + vim keys
hl.bind(keys(mod, vleft), hl.dsp.focus({ direction = "l" }))
hl.bind(keys(mod, vright), hl.dsp.focus({ direction = "r" }))
-- hl.bind(keys(mod, vup), hl.dsp.focus({ direction = "u" }))
-- hl.bind(keys(mod, vdown), hl.dsp.focus({ direction = "d" }))
hl.bind(keys(mod, vup), hl.dsp.window.cycle_next({ next = false }))
hl.bind(keys(mod, vdown), hl.dsp.window.cycle_next())

-- hl.bind(keys(alt, "Tab"), hl.dsp.exec_cmd("..."))
hl.bind(keys(alt, "tab"), hl.dsp.exec_cmd("~/.config/hypr/scripts/alt_tab_global.sh"))

--------------------------------------

hl.bind(keys(mod, shift, vleft), hl.dsp.window.move({ direction = "l" }))
hl.bind(keys(mod, shift, vright), hl.dsp.window.move({ direction = "r" }))
-- hl.bind(keys(mod, shift, vup), hl.dsp.window.move({ direction = "u" }))
-- hl.bind(keys(mod, shift, vdown), hl.dsp.window.move({ direction = "d" }))
hl.bind(keys(mod, shift, vup), hl.dsp.window.swap({ prev = true }))
hl.bind(keys(mod, shift, vdown), hl.dsp.window.swap({ next = true }))

-- Monitors
hl.bind(keys(mod, "Period"), hl.dsp.focus({ monitor = "+1" }))
hl.bind(keys(mod, "Comma"), hl.dsp.focus({ monitor = "-1" }))

-- Scroll through existing workspaces with mod + scroll
-- hl.bind(keys(mod, "mouse_down"), hl.dsp.focus({ workspace = "e+1" }))
-- hl.bind(keys(mod, "mouse_up"), hl.dsp.focus({ workspace = "e-1" }))

-- Move/resize windows with mod + LMB/RMB and dragging
hl.bind(keys(mod, "mouse:272"), hl.dsp.window.drag(), { mouse = true })
hl.bind(keys(mod, "mouse:273"), hl.dsp.window.resize(), { mouse = true })

----------------------------------------------------
----------------------- Resize ---------------------
----------------------------------------------------

hl.bind(keys(ctrl, mod, vright), hl.dsp.window.resize({ x = 100, y = 0, relative = true }), { repeating = true })
hl.bind(keys(ctrl, mod, vleft), hl.dsp.window.resize({ x = -100, y = 0, relative = true }), { repeating = true })
hl.bind(keys(ctrl, mod, vup), hl.dsp.window.resize({ x = 0, y = -100, relative = true }), { repeating = true })
hl.bind(keys(ctrl, mod, vdown), hl.dsp.window.resize({ x = 0, y = 100, relative = true }), { repeating = true })

----------------------------------------------------
----------------------- Apps -----------------------
----------------------------------------------------

-- Start a terminal
hl.bind(keys(mod, "RETURN"), hl.dsp.exec_cmd(terminal))

-- Open/close gnome-control-center
hl.bind(keys(alt, shift, "G"), hl.dsp.exec_cmd("kill -9 $(pidof gnome-control-center) || XDG_CURRENT_DESKTOP=GNOME gnome-control-center"))
hl.bind(keys(alt, shift, "W"), hl.dsp.exec_cmd("~/.config/waypaper/toggle.sh hyprland"))
hl.bind(keys(alt, shift, "B"), hl.dsp.exec_cmd("~/dotfiles/bin/start-browserw"))

hl.bind("Print", hl.dsp.exec_cmd("~/dotfiles/bin/screenshot.w.sh"))
hl.bind("Pause", hl.dsp.exec_cmd("~/dotfiles/bin/screenshot.w.sh"))

------------------------------------------------------
----------------------- Layouts ----------------------
------------------------------------------------------

-- hl.bind(keys(alt, "S"), hl.dsp.window.float({ action = "toggle" }))
-- hl.bind(keys(mod, shift, "P"), hl.dsp.exec_cmd("hyprctl dispatch togglefloating && hyprctl dispatch centerwindow"))
hl.bind(keys(mod, "P"), hl.dsp.window.pseudo()) -- dwindle
hl.bind(keys(mod, "S"), hl.dsp.layout("togglesplit")) -- dwindle
hl.bind(keys(alt, "S"), hl.dsp.layout("swapsplit"))

-- hl.bind(keys(mod, "F"), hl.dsp.window.fullscreen({ mode = "maximized" })) -- monocle
hl.bind(keys(mod, "F"), hl.dsp.group.toggle()) -- tabbed
hl.bind(keys(mod, "code:49"), hl.dsp.focus({ last = true })) -- focuscurrentorlast
hl.bind(keys(mod, "code:59"), hl.dsp.group.prev()) -- b - back
hl.bind(keys(mod, "code:60"), hl.dsp.group.next()) -- f - forward
hl.bind(keys(mod, shift, "code:59"), hl.dsp.window.move({ into_group = "l" })) -- Moves the active window into a group left
hl.bind(keys(mod, shift, "code:60"), hl.dsp.window.move({ into_group = "r" })) -- Moves the active window into a group right

hl.bind(keys(mod, shift, "F"), hl.dsp.window.fullscreen({ mode = "fullscreen" }))

-- hl.bind(keys(mod, "Tab"), hl.dsp.exec_cmd("~/.config/hypr/shell/toggle_layout.sh"))
hl.bind(keys(mod, "Tab"), hl.dsp.window.fullscreen({ mode = "maximized" }))

----------------------------------------------------
----------------------- Games ----------------------
----------------------------------------------------

hl.bind(keys(mod, "F1"), hl.dsp.exec_cmd("~/.config/hypr/scripts/gamemode.sh"))

----------------------------------------------------
----------------------- Media ----------------------
----------------------------------------------------

-- Monitor brightness control
hl.bind(keys(mod, "Page_Up"), hl.dsp.exec_cmd("echo '+' > /tmp/waybar-ddc-module-rx"))
hl.bind(keys(mod, "Page_Down"), hl.dsp.exec_cmd("echo '-' > /tmp/waybar-ddc-module-rx"))
hl.bind(keys(mod, shift, "Page_Down"), hl.dsp.exec_cmd("echo '=' > /tmp/waybar-ddc-module-rx"))

-- Monitor color temperature control
hl.bind(keys(mod, "Home"), hl.dsp.exec_cmd("~/.config/waybar/module/shell/module.hyprsunset + ; pkill -RTMIN+4 waybar"))
hl.bind(keys(mod, "End"), hl.dsp.exec_cmd("~/.config/waybar/module/shell/module.hyprsunset - ; pkill -RTMIN+4 waybar"))
hl.bind(keys(mod, shift, "Home"), hl.dsp.exec_cmd("~/.config/waybar/module/shell/module.hyprsunset = ; pkill -RTMIN+4 waybar"))
hl.bind(keys(mod, shift, "End"), hl.dsp.exec_cmd("~/.config/waybar/module/shell/module.hyprsunset bblock ; pkill -RTMIN+4 waybar"))

-- Volume control
hl.bind(keys(mod, "left"), hl.dsp.exec_cmd("wpctl set-volume @DEFAULT_AUDIO_SINK@ 5%-"))
hl.bind(keys(mod, "right"), hl.dsp.exec_cmd("wpctl set-volume -l 1 @DEFAULT_AUDIO_SINK@ 5%+"))
hl.bind(keys(mod, "down"), hl.dsp.exec_cmd("wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle"))

-- Player control
hl.bind(keys(mod, shift, "left"), hl.dsp.exec_cmd("playerctl previous"))
hl.bind(keys(mod, shift, "right"), hl.dsp.exec_cmd("playerctl next"))
hl.bind(keys(mod, shift, "down"), hl.dsp.exec_cmd("playerctl play-pause"))

-- MPD control
hl.bind(keys(mod, ctrl, "down"), hl.dsp.exec_cmd("mpc toggle"))
hl.bind(keys(mod, ctrl, "up"), hl.dsp.exec_cmd("mpc stop"))
hl.bind(keys(mod, ctrl, "right"), hl.dsp.exec_cmd("mpc next"))
hl.bind(keys(mod, ctrl, "left"), hl.dsp.exec_cmd("mpc prev"))

----------------------------------------------------
-------------------- Workspace ---------------------
----------------------------------------------------

-- Switch workspaces with mod + [1-9]
-- Move active window to a workspace with mod + SHIFT + [1-9]
for i = 1, 9 do
    hl.bind(keys(mod, tostring(i)), hl.dsp.focus({ workspace = i }))
    hl.bind(keys(mod, shift, tostring(i)), hl.dsp.window.move({ workspace = i }))
end

-- Special workspace (scratchpad)
-- hl.bind(keys(mod, "O"), hl.dsp.workspace.toggle_special("scratchpad"))
-- hl.bind(keys(mod, shift, "O"), hl.dsp.window.move({ workspace = "special:scratchpad" }))
hl.bind(keys(mod, shift, "S"), hl.dsp.workspace.toggle_special("scratchpad"))

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

hl.bind(keys(mod, "equal"), hl.dsp.exec_cmd("hyprctl -q keyword cursor:zoom_factor $(hyprctl getoption cursor:zoom_factor | awk '/^float.*/ {print $2 * 1.1}')"))
hl.bind(keys(mod, "minus"), hl.dsp.exec_cmd("hyprctl -q keyword cursor:zoom_factor $(hyprctl getoption cursor:zoom_factor | awk '/^float.*/ {print $2 * 0.9}')"))
hl.bind(keys(mod, shift, "minus"), hl.dsp.exec_cmd("hyprctl -q keyword cursor:zoom_factor 1"))
