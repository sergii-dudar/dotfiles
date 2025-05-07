
case "$1" in
    "yazi")
        # xprop WM_CLASS: WM_CLASS(STRING) = "ghostty", "com.ghostty.group01"
        instance_name="ghostty"
        class_name="com.scratchpad.yazi"
        run_cmd="ghostty --class=com.scratchpad.yazi -e yazi"
        notify_msg="📂 Yazi File Manager"
        ;;
    "nautilus")
        instance_name="org.gnome.Nautilus"
        class_name="org.gnome.Nautilus"
        run_cmd="nautilus"
        notify_msg="📂 Nautilus"
        ;;
    "telegram")
        instance_name="telegram-desktop"
        class_name="TelegramDesktop"
        run_cmd="telegram-desktop"
        notify_msg="💬 Telegram"
        ;;
    "youtube_music")
        instance_name="crx_cinhimbnkkaeohfgghhklpknlkffjgod"
        class_name="Brave-browser"
        run_cmd="brave --profile-directory=Default --app-id=cinhimbnkkaeohfgghhklpknlkffjgod"
        notify_msg="💽 Music"
        ;;
    "google_chat")
        instance_name="crx_mdpkiolbdkhdjpekfbkbmhigcaggjagi"
        class_name="Brave-browser"
        run_cmd="brave --profile-directory=Default --app-id=mdpkiolbdkhdjpekfbkbmhigcaggjagi"
        notify_msg="✉️  Google Chat"
        ;;
    "monkey_type")
        instance_name="crx_picebhhlijnlefeleilfbanaghjlkkna"
        class_name="Brave-browser"
        run_cmd="brave --profile-directory=Default --app-id=picebhhlijnlefeleilfbanaghjlkkna"
        notify_msg="  Monkey Type"
        ;;
    *)
        echo "$1 - is not supported"
        exit 1
        ;;
esac

APP_ID="$1"
APP_CMD="$2"
MATCH_PROP="app_id=$APP_ID"

# Check if the app is already running
notify-send "1"
if ! pgrep --full "$APP_CMD" >/dev/null; then
    bash -c "$APP_CMD" > /dev/null 2>&1 &
    # Wait until the window is available to sway
    for i in {1..20}; do
        swaymsg -t get_tree | grep -q "$APP_ID" && break
        sleep 0.25
    done
    notify-send "runned"
fi

notify-send "2"
# Optionally, move to scratchpad if not there yet
if swaymsg -t get_tree | jq -e '.. | objects | select(.app_id? == "'"$APP_ID"'") | select(.scratchpad_state == "none")' >/dev/null; then
    notify-send "matched"
    swaymsg [app_id="$APP_ID"] floating enable, move to scratchpad
    sleep 0.5
fi

notify-send "3"
# Finally, show the app from scratchpad
swaymsg [app_id="$APP_ID"] scratchpad show, border pixel 5, resize set 75ppt 80ppt, move position center