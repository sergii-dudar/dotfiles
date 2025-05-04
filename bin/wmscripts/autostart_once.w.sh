#!/usr/bin/env bash

wm_name="${1:-}"

#~/dotfiles/bin/apply-display-settings.sh
ssh-add &
swaync &  # dunst &
/usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1 &

# Automatically lock the screen after 10 minutes of inactivity
#xautolock -time 10 -locker "$HOME/dotfiles/bin/screen-lock" -detectsleep &

#nm-applet &

# Global config of wm
case "$wm_name" in
    sway)
        # killall sxhkd; sxhkd -c ~/.config/sxhkd/i3/sxhkdrc &

        # Start the daemon which listens to focus changes and sets _back mark
        i3-back &
        # autotilin &

        # run scractchpad apps (i3 can't open them automatically)
        brave &
        telegram-desktop &
        #nautilus &
        #ghostty --class=com.scratchpad.yazi -e yazi &
        ghostty --class=com.ghostty.group01 &

        # youtube_music
        #brave --profile-directory=Default --app-id=cinhimbnkkaeohfgghhklpknlkffjgod &
        # google_chat
        #brave --profile-directory=Default --app-id=mdpkiolbdkhdjpekfbkbmhigcaggjagi &
        # monkey_type
        #brave --profile-directory=Default --app-id=picebhhlijnlefeleilfbanaghjlkkna &

        ;;
    *)
        #killall sxhkd; sxhkd -c ~/.config/sxhkd/sxhkdrc &
        ;;
esac

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