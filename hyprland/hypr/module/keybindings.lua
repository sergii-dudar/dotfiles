-----------------------
------ Variables ------
-----------------------

local terminal = "foot" -- "ghostty", "alacritty"
local fileManager = "nautilus"
local menu = "~/.config/rofi/scripts/launcher_t1"
local qmenu = "~/.config/rofi/scripts/powermenu_t1"
local clipmenu = "~/.config/rofi/cliphist/cliphist-menu"
local killmenu = "~/.config/rofi/killmenu/kill-menu"

local mainMod = "SUPER" -- Sets "Windows" key as main modifier
local alt = "ALT"
local shift = "SHIFT"
local ctrl = "Control_L" -- Control
local tab = "tab"

local vleft = "h"
local vdown = "j"
local vup = "k"
local vright = "l"

function join(...)
    return table.concat({...}, " + ")
end

---------------------
------- Basics ------
---------------------

-- Kill focused window
hl.bind(join(mainMod, shift, "C"), hl.dsp.window.kill())
hl.bind(join(mainMod, ctrl, "C"), hl.dsp.exec_cmd(killmenu))

-- Start your launcher
hl.bind(join(alt, "SPACE"), hl.dsp.exec_cmd(menu))
hl.bind(join(mainMod, "Q"), hl.dsp.exec_cmd(qmenu))
hl.bind(join(mainMod, "V"), hl.dsp.exec_cmd(clipmenu))

-- Other
-- hl.bind("$ctrl+$alt, "Q", hl.dsp.exec_cmd(~/dotfiles/bin/screen-lockw
hl.bind(join(ctrl, alt, "Q"), hl.dsp.exec_cmd("hyprlock"))
hl.bind(join(mainMod, "SPACE"), hl.dsp.exec_cmd("~/.config/waybar/module/shell/change_language.sh hyprland"))
hl.bind(join(mainMod, shift, "R"), hl.dsp.exec_cmd("hyprctl reload"))

-- toggle waybar visibility
hl.bind(join(mainMod, "B"), hl.dsp.exec_cmd("killall -SIGUSR1 waybar"))

---------------------
----- Navigation ----
---------------------

-- Move focus with mainMod + vim keys
hl.bind(join(mainMod, vleft), hl.dsp.focus({ direction = "left" }))
hl.bind(join(mainMod, vright), hl.dsp.focus({ direction = "right" }))
-- hl.bind(join(mainMod, vup), hl.dsp.focus({ direction = "up" }))
-- hl.bind(join(mainMod, vdown), hl.dsp.focus({ direction = "down" }))
hl.bind(join(mainMod, vup), cyclenext, prev)
hl.bind(join(mainMod, vdown), cyclenext)

-- hl.bind("$alt, Tab, cyclenext, next tiled hist        # change focus to another window
-- hl.bind("$alt, Tab, bringactivetotop,    # bring it to the top
hl.bind(join(alt, tab), hl.dsp.exec_cmd("~/.config/hypr/scripts/alt_tab_global.sh"))

--------------------------------------

hl.bind(join(mainMod, shift, vleft), movewindow, l)
hl.bind(join(mainMod, shift, vright), movewindow, r)
-- hl.bind(join("mainMod+$shift, $vup, movewindow, u
-- hl.bind(join("mainMod+$shift, $vdown, movewindow, d
hl.bind(join(mainMod, shift, vup), swapnext, prev)
hl.bind(join(mainMod, shift, vdown), swapnext)

-- Monitors
hl.bind(join(mainMod, "Period"), focusmonitor, +1)
hl.bind(join(mainMod, "Comma"), focusmonitor, -1)

-- Scroll through existing workspaces with mainMod + scroll
-- hl.bind(join("mainMod, mouse_down, workspace, e+1
-- hl.bind(join("mainMod, mouse_up, workspace, e-1

-- Move/resize windows with mainMod + LMB/RMB and dragging
bindm = mainMod, mouse:272, movewindow
bindm = mainMod, mouse:273, resizewindow

---------------------
------- Resize ------
---------------------

binde = $ctrl+mainMod, $vright, resizeactive, 100 0
binde = $ctrl+mainMod, $vleft, resizeactive, -100 0
binde = $ctrl+mainMod, $vup, resizeactive, 0 -100
binde = $ctrl+mainMod, $vdown, resizeactive, 0 100

---------------------
-------- Apps -------
---------------------

-- Start a terminal
hl.bind(join(mainMod, "RETURN"), hl.dsp.exec_cmd(terminal))

-- open/close gnome-control-center
hl.bind(join(alt, shift, "G"), hl.dsp.exec_cmd("kill -9 $(pidof gnome-control-center) || XDG_CURRENT_DESKTOP=GNOME gnome-control-center"))
hl.bind(join(alt, shift, "W"), hl.dsp.exec_cmd("~/.config/waypaper/toggle.sh hyprland"))
hl.bind(join(alt, shift, "B"), hl.dsp.exec_cmd("~/dotfiles/bin/start-browserw"))

hl.bind("Print", hl.dsp.exec_cmd("~/dotfiles/bin/screenshot.w.sh"))

---------------------
------ Layouts ------
---------------------

-- hl.bind("$alt, S, togglefloating,
-- hl.bind("mainMod+$shift, P, hl.dsp.exec_cmd(hyprctl dispatch togglefloating && hyprctl dispatch centerwindow
-- hl.bind("$alt, S, hl.dsp.exec_cmd(hyprctl dispatch togglefloating && hyprctl dispatch centerwindow
hl.bind(join(mainMod, "P"), pseudo) -- dwindle
hl.bind(join(mainMod, "S"), layoutmsg, togglesplit) -- dwindle
hl.bind(join(alt, "S"), layoutmsg, swapsplit)

-- hl.bind("mainMod, F, fullscreen, 1 # monocle
hl.bind(join(mainMod, "F"), togglegroup -- tabbed
hl.bind("mainMod, code:49, focuscurrentorlast) -- b - back, f - forward, or index start at 1
hl.bind("mainMod, code:59, changegroupactive, b) -- b - back, f - forward, or index start at 1
hl.bind("mainMod, code:60, changegroupactive, f) -- b - back, f - forward, or index start at 1
hl.bind("mainMod+$shift, code:59, moveintogroup, l) -- Moves the active window into a group left
hl.bind("mainMod+$shift, code:60, moveintogroup, r) -- Moves the active window into a group right

hl.bind(join(mainMod, shift, "F"), fullscreen, 0)

-- hl.bind("mainMod, "Tab", hl.dsp.exec_cmd(~/.config/hypr/shell/toggle_layout.sh
hl.bind(join(mainMod, "Tab"), fullscreen, 1)

---------------------
------- Games -------
---------------------

hl.bind(join(mainMod, "F1"), hl.dsp.exec_cmd("~/.config/hypr/scripts/gamemode.sh"))

---------------------
------- Media -------
---------------------

-- Laptop multimedia keys for volume and LCD brightness
-- bindel = ,XF86MonBrightnessUp, hl.dsp.exec_cmd(brightnessctl -e4 -n2 set 5%+
-- bindel = ,XF86MonBrightnessDown, hl.dsp.exec_cmd(brightnessctl -e4 -n2 set 5%-

-- hl.bind("mainMod, code:20, hl.dsp.exec_cmd(ddcutil setvcp 10 - 10
-- hl.bind("mainMod, code:21, hl.dsp.exec_cmd(ddcutil setvcp 10 + 10
-- hl.bind("mainMod, minus, hl.dsp.exec_cmd(ddcutil setvcp 10 - 10
-- hl.bind("mainMod, equal, hl.dsp.exec_cmd(ddcutil setvcp 10 + 10

-- monitor brightness controll
hl.bind(join(mainMod, "Page_Up"), hl.dsp.exec_cmd("echo '+' > /tmp/waybar-ddc-module-rx")
hl.bind(join(mainMod, "Page_Down"), hl.dsp.exec_cmd("echo '-' > /tmp/waybar-ddc-module-rx")
hl.bind(join(mainMod, shift, "Page_Down"), hl.dsp.exec_cmd("echo '=' > /tmp/waybar-ddc-module-rx")

-- monitor color temperature controll
hl.bind(join(mainMod, "Home"), hl.dsp.exec_cmd("~/.config/waybar/module/shell/module.hyprsunset + ; pkill -RTMIN+4 waybar")
hl.bind(join(mainMod, "End"), hl.dsp.exec_cmd("~/.config/waybar/module/shell/module.hyprsunset - ; pkill -RTMIN+4 waybar")
hl.bind(join(mainMod, shift, "Home"), hl.dsp.exec_cmd("~/.config/waybar/module/shell/module.hyprsunset = ; pkill -RTMIN+4 waybar")
hl.bind(join(mainMod, shift, "End"), hl.dsp.exec_cmd("~/.config/waybar/module/shell/module.hyprsunset bblock ; pkill -RTMIN+4 waybar")

-- sudo ddcutil getvcp 10
-- ddcutil getvcp 10 --display 1 #VCP code 0x10 (Brightness): current value = 40, max value = 100 
-- ddcutil getvcp 10 --display 2 #VCP code 0x10 (Brightness): current value = 56, max value = 100

hl.bind(join(mainMod, "left"), hl.dsp.exec_cmd("wpctl set-volume @DEFAULT_AUDIO_SINK@ 5%-")
hl.bind(join(mainMod, "right"), hl.dsp.exec_cmd("wpctl set-volume -l 1 @DEFAULT_AUDIO_SINK@ 5%+")
hl.bind(join(mainMod, "down"), hl.dsp.exec_cmd("wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle")
-- bindel = ,XF86AudioMicMute, hl.dsp.exec_cmd(wpctl set-mute @DEFAULT_AUDIO_SOURCE@ toggle

hl.bind(join(mainMod, shift, "deft"), hl.dsp.exec_cmd("playerctl previous")
hl.bind(join(mainMod, shift, "right"), hl.dsp.exec_cmd("playerctl next")
hl.bind(join(mainMod, shift, "down"), hl.dsp.exec_cmd("playerctl play-pause")

hl.bind(join(mainMod, ctrl, "down"), hl.dsp.exec_cmd("mpc toggle"))
hl.bind(join(mainMod, ctrl, "up"), hl.dsp.exec_cmd("mpc stop"))
hl.bind(join(mainMod, ctrl, "right"), hl.dsp.exec_cmd("mpc next"))
hl.bind(join(mainMod, ctrl, "left"), hl.dsp.exec_cmd("mpc prev"))

---------------------
----- Workspace -----
---------------------

-- Switch workspaces with mainMod + [0-9]
-- Move active window to a workspace with mainMod + SHIFT + [0-9]
local ws_number = 9
for i = 1, ws_number do
    local key = i % ws_number -- ws_number maps to key 0
    hl.bind(mainMod .. " + " .. key, hl.dsp.focus({ workspace = i }))
    hl.bind(mainMod .. " + SHIFT + " .. key, hl.dsp.window.move({ workspace = i }))
end

-- Example special workspace (scratchpad)
hl.bind(mainMod .. " + S", hl.dsp.workspace.toggle_special("scratchpad"))
-- hl.bind(mainMod .. " + SHIFT + S", hl.dsp.window.move({ workspace = "special:scratchpad" }))

-- Scroll through existing workspaces with mainMod + scroll
hl.bind(mainMod .. " + mouse_down", hl.dsp.focus({ workspace = "e+1" }))
hl.bind(mainMod .. " + mouse_up", hl.dsp.focus({ workspace = "e-1" }))

-- Move/resize windows with mainMod + LMB/RMB and dragging
hl.bind(mainMod .. " + mouse:272", hl.dsp.window.drag(), { mouse = true })
hl.bind(mainMod .. " + mouse:273", hl.dsp.window.resize(), { mouse = true })

---------------------
------- Submap ------
---------------------

-- Switch to a submap called `resize`.
hl.bind("ALT + R", hl.dsp.submap("resize"))

-- Start a submap called "resize".
hl.define_submap("resize", function()

    -- Set repeating binds for resizing the active window.
    hl.bind("right", hl.dsp.window.resize({ x = 10, y = 0, relative = true}), { repeating = true })
    hl.bind("left", hl.dsp.window.resize({ x = -10, y = 0, relative = true}), { repeating = true })
    hl.bind("up", hl.dsp.window.resize({ x = 0, y = 10, relative = true}), { repeating = true })
    hl.bind("down", hl.dsp.window.resize({ x = 0, y = -10, relative = true}), { repeating = true })

    -- Use `reset` to go back to the global submap
    hl.bind("escape", hl.dsp.submap("reset"))

end)

---------------------
-------- Zoom -------
---------------------

-- bind = $mod, equal, exec, hyprctl -q keyword cursor:zoom_factor $(hyprctl getoption cursor:zoom_factor | awk '/^float.*/ {print $2 * 1.1}')
-- bind = $mod, minus, exec, hyprctl -q keyword cursor:zoom_factor $(hyprctl getoption cursor:zoom_factor | awk '/^float.*/ {print $2 * 0.9}')
-- bind = $mod+$shift, minus, exec, hyprctl -q keyword cursor:zoom_factor 1

---------------------
---- ........... ----
---------------------