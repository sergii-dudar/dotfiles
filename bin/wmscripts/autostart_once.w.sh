#!/usr/bin/env bash

wm_name="${1:-}"

#~/dotfiles/bin/apply-display-settings.sh
ssh-add &
swaync &  # dunst &

# Automatically lock the screen after 10 minutes of inactivity
#xautolock -time 10 -locker "$HOME/dotfiles/bin/screen-lock" -detectsleep &

#nm-applet &
wl-paste --type text --watch cliphist store & # Stores only text data

# Global config of wm
case "$wm_name" in
    sway)
        /usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1 &
        # killall sxhkd; sxhkd -c ~/.config/sxhkd/i3/sxhkdrc &
        run_swayidle 'swaymsg "output * dpms off"' 'swaymsg "output * dpms on"'
        # Start the daemon which listens to focus changes and sets _back mark
        i3-back &
        # autotilin &

        # brave &
        # ghostty --class=com.ghostty.group01 &

        (sleep 0.4 && ghostty --class=com.ghostty.group01) &
        # (sleep 0.7 && brave --force-device-scale-factor=1.2) &
        (sleep 0.7 && brave --enable-features=UseOzonePlatform,WaylandWindowDecorations --ozone-platform=wayland) &

        ;;
    hyprland)
        # systemctl --user --now enable hyprpolkitagent

        ghostty --class=com.ghostty.group01 &
        # brave --force-device-scale-factor=1.2 &
        brave --enable-features=UseOzonePlatform,WaylandWindowDecorations --ozone-platform=wayland &
        waypaper --restore --backend hyprpaper &

        if hyprctl monitors | grep -q "HDMI-A-3"; then
            hyprctl dispatch workspace 9 && \
                hyprctl dispatch moveworkspacetomonitor 9 HDMI-A-3 && \
                hyprctl dispatch workspace 1
        fi

        # hyprland
        # run_swayidle 'hyprctl dispatch dpms off' 'hyprctl dispatch dpms on'
        #killall sxhkd; sxhkd -c ~/.config/sxhkd/sxhkdrc &
        ;;
esac

function run_swayidle() {
    swayidle -w \
        timeout 600 "$HOME/dotfiles/bin/screen-lockw" \
        timeout 1200 "$1" \
        resume "$2" \
        before-sleep "$HOME/dotfiles/bin/screen-lockw" &
}

# case "$XDG_SESSION_TYPE" in
#     wayland)
#         # echo "Running on Wayland"
#         ;;
#     x11)
#         # 200 → Delay before key repeat starts (in milliseconds).
#         # 50 → Repeat rate (keys per second).
#         xset r rate 200 30
#         ;;
#     *)
#         # echo "Unknown session type"
#         ;;
# esac