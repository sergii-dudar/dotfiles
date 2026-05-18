----------------------------------------------
----------- WINDOWS AND WORKSPACES -----------
----------------------------------------------

-- See https://wiki.hypr.land/Configuring/Basics/Window-Rules/
-- See https://wiki.hypr.land/Configuring/Basics/Workspace-Rules/

-- Ignore maximize requests from all apps. You'll probably like this.
hl.window_rule({
    name = "suppress-maximize-events",
    match = { class = ".*" },
    suppress_event = "maximize",
    -- group = "set always",
})

-- Fix some dragging issues with XWayland
hl.window_rule({
    name = "fix-xwayland-drags",
    match = {
        class = "^$",
        title = "^$",
        xwayland = true,
        float = true,
        fullscreen = false,
        pin = false,
    },
    no_focus = true,
})

-- Hyprland-run windowrule
hl.window_rule({
    name = "move-hyprland-run",
    match = { class = "hyprland-run" },
    move = "20 monitor_h-120",
    float = true,
})

------------------------------------------------------------
-------- Open applications on specific workspaces ----------
------------------------------------------------------------

hl.window_rule({
    name = "wk1",
    match = { class = "(com.ghostty.group01|org.wezfurlong.wezterm|com.alacritty.group01|com.term.group01)" },
    workspace = "1 silent",
})

hl.window_rule({
    name = "wk2",
    match = { class = "(jetbrains-idea|Code)" },
    workspace = "2 silent",
})

hl.window_rule({
    name = "wk3",
    match = { class = "(brave-browser)" },
    workspace = "2 silent",
})

hl.window_rule({
    name = "wk4",
    match = { class = "(kitty)" },
    workspace = "4 silent",
})

------------------------------------------------------------
------- Open specific applications in floating mode --------
------------------------------------------------------------
-- #4db5bd #c678dd #A3BE8C #81A1C1 #B48EAD #88C0D0 #4C566A #A3BE8C #81A1C1
-- #B48EAD #8FBCBB #458588 #689d6a #a89984 #928374 #83a598 #8ec07c

local float_border_color = "rgb(81A1C1)"

local float_apps = "("
    .. "org.qbittorrent.qBittorrent|"
    .. "org.gnome.Settings|"
    .. "nm-connection-editor|"
    .. "com.viber.Viber|"
    .. "vlc|"
    .. "org.gnome.Calculator|"
    .. "org.gnome.Snapshot|"
    .. "org.gnome.clocks|"
    .. "org.gnome.Calendar|"
    .. "org.gnome.Weather|"
    .. "org.gnome.DiskUtility|"
    .. "org.gnome.SystemMonitor|"
    .. "com.term.float.htop_info|"
    .. "com.term.float.disc_ugd"
    .. ")"

-- stylua: ignore start
local ghostty_tui_apps = "("
    .. "com.scratchpad.yazi|"
    .. "com.scratchpad.music|"
    .. "com.term.float.htop_info|"
    .. "com.term.float.disc_ugd"
    .. ")"

local float_small_apps = "(" 
    .. "org.pulseaudio.pavucontrol|" 
    .. "xdg-desktop-portal-gtk|" 
    .. "waypaper" 
    .. ")"
-- stylua: ignore end

hl.window_rule({
    name = "float_apps_role",
    match = { class = float_apps },
    float = true,
    size = "(monitor_w*0.65) (monitor_h*0.8)",
    center = true,
    border_color = float_border_color,
    opacity = "0.97",
    -- dim_around = true,
})

hl.window_rule({
    name = "float_small_apps_role",
    match = { class = float_small_apps },
    float = true,
    size = "(monitor_w*0.45) (monitor_h*0.5)",
    center = true,
    border_color = float_border_color,
    opacity = "0.97",
})

hl.window_rule({
    name = "ghostty_tui_apps_role",
    match = { class = ghostty_tui_apps },
    opacity = "0.90",
})

-- alacritty (xdg_shell)
-- alacritty --class=com.term.float.htop_info -e htop
-- alacritty --class=com.term.float.disc_ugd -e gdu ~

--------------------------------------------------------
------------------- Dynamic roles ----------------------
--------------------------------------------------------

-- stop hypridle when fullscreen
hl.window_rule({
    name = "idleinhibit_mpv",
    match = { class = "mpv" },
    idle_inhibit = "focus",
    fullscreen_state = "2",
})

hl.window_rule({
    name = "idleinhibit_fullscreen",
    match = { fullscreen_state_internal = 2 },
    idle_inhibit = "fullscreen",
})

--------------------------------------------------------
------------------- Workspace roles --------------------
--------------------------------------------------------

---------- smart borders
hl.workspace_rule({ workspace = "w[tv1]" })
hl.workspace_rule({ workspace = "f[1]" })

hl.window_rule({
    name = "smart-border-wtv1",
    match = { float = false, workspace = "w[tv1]" },
    border_size = 0,
    rounding = 0,
})

hl.window_rule({
    name = "smart-border-f1",
    match = { float = false, workspace = "f[1]" },
    border_size = 0,
    rounding = 0,
})

--------------------------------------------------------
-------------------- Games roles -----------------------
--------------------------------------------------------

hl.window_rule({
    name = "dota_win_rule",
    match = { class = "^(dota2)$" },
    immediate = true,
})
