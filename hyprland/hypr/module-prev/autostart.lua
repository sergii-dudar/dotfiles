-------------------
---- AUTOSTART ----
-------------------

-- See https://wiki.hypr.land/Configuring/Basics/Autostart/

-- Autostart necessary processes (like notifications daemons, status bars, etc.)
-- Or execute your favorite apps at launch like this:

hl.on("hyprland.start", function()
    -- hl.exec_cmd(terminal)
    -- hl.exec_cmd("nm-applet")
    -- hl.exec_cmd("waybar & hyprpaper & firefox")

    -- exec-once = dbus-update-activation-environment --systemd HYPRLAND_INSTANCE_SIGNATURE
    hl.exec_cmd("dbus-update-activation-environment --systemd --all")
    hl.exec_cmd("~/dotfiles/bin/wmscripts/autostart_once.w.sh hyprland")
    hl.exec_cmd("hypridle")
    hl.exec_cmd("hyprsunset")

    -- exec-once = hyprpaper # some issue with deps, temp deleted, try latter: sudo pacman -S hyprpaper
    -- exec-once = sleep 0.5 && waypaper --restore --backend hyprpaper
    hl.exec_cmd("swaybg -o HDMI-A-1 -i ~/wallpapers/jpg/Wall147.jpg -m fill")
    hl.exec_cmd("swaybg -o HDMI-A-2 -i ~/wallpapers/jpg/Wall147.jpg -m fill")
    hl.exec_cmd("swaybg -o HDMI-A-3 -i ~/wallpapers/portrait/AzZxWk0.png -m fill")
end)

hl.exec_cmd("hyprctl setcursor elementary 30")
hl.exec_cmd("~/dotfiles/bin/apply-display-settings.w.sh hyprland")
hl.exec_cmd("~/.config/waybar/hyprland-waybar-run")
hl.exec_cmd("~/dotfiles/bin/wmscripts/autostart_always.w.sh hyprland")
-- exec = sleep 3 && ~/dotfiles/hyprland/hypr/scripts/hyprsunset.runner
-- exec = /usr/lib/xdg-desktop-portal -r
