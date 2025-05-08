function hide_all_visible_scratchpads() {
    swaymsg -t get_tree |
    jq -r '.. | objects | select(.scratchpad_state? == "fresh" and .visible == true) | .id' |
    while read -r win_id; do
        swaymsg [con_id="$win_id"] scratchpad show #move to scratchpad
    done
}
hide_all_visible_scratchpads
sleep 0.2

case "$1" in
    "yazi")
        shell="xdg_shell"
        app_id="com.scratchpad.yazi"
        run_cmd="ghostty --class=com.scratchpad.yazi -e yazi"
        notify_msg="📂 Yazi File Manager"
        ;;
    "nautilus")
        shell="xdg_shell"
        app_id="org.gnome.Nautilus"
        run_cmd="nautilus"
        notify_msg="📂 Nautilus"
        ;;
    "telegram")
        shell="xdg_shell"
        app_id="org.telegram.desktop"
        run_cmd="telegram-desktop"
        notify_msg="💬 Telegram"
        ;;
    "youtube_music")
        shell="xwayland"
        instance_name="crx_cinhimbnkkaeohfgghhklpknlkffjgod"
        class_name="Brave-browser"
        run_cmd="brave --profile-directory=Default --app-id=cinhimbnkkaeohfgghhklpknlkffjgod"
        notify_msg="💽 Music"
        ;;
    "google_chat")
        shell="xwayland"
        instance_name="crx_mdpkiolbdkhdjpekfbkbmhigcaggjagi"
        class_name="Brave-browser"
        run_cmd="brave --profile-directory=Default --app-id=mdpkiolbdkhdjpekfbkbmhigcaggjagi"
        notify_msg="✉️  Google Chat"
        ;;
    "monkey_type")
        shell="xwayland"
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

# if ! pgrep --full "$run_cmd" >/dev/null; then
# fi


if [ $shell = "xwayland" ]; then

    if ! swaymsg -t get_tree | grep -q "$instance_name"; then
        bash -c "$run_cmd" > /dev/null 2>&1 &

        # Wait until the window is available to sway
        for i in {1..20}; do
            swaymsg -t get_tree | grep -q "$instance_name" && break
            sleep 0.25
        done
    fi

    if swaymsg -t get_tree | jq -e '.. | objects | select(.window_properties.class? == "'"$class_name"'" and .window_properties.instance? == "'"$instance_name"'") | select(.scratchpad_state == "none")' >/dev/null; then
        # move to scratchpad if not there yet
        swaymsg [class="$class_name" instance="$instance_name"] floating enable, move to scratchpad
        sleep 0.5
    fi

    # show the app from scratchpad
    swaymsg [class="$class_name" instance="$instance_name"] scratchpad show, border pixel 5, resize set 75ppt 80ppt, move position center
    notify-send "$notify_msg" -t 700
else

    if ! swaymsg -t get_tree | grep -q "$app_id"; then
        bash -c "$run_cmd" > /dev/null 2>&1 &

        # Wait until the window is available to sway
        for i in {1..20}; do
            swaymsg -t get_tree | grep -q "$app_id" && break
            sleep 0.25
        done
    fi


    if swaymsg -t get_tree | jq -e '.. | objects | select(.app_id? == "'"$app_id"'") | select(.scratchpad_state == "none")' >/dev/null; then
        # move to scratchpad if not there yet
        swaymsg [app_id="$app_id"] floating enable, move to scratchpad
        sleep 0.5
    fi

    # show the app from scratchpad
    swaymsg [app_id="$app_id"] scratchpad show, border pixel 5, resize set 75ppt 80ppt, move position center
    notify-send "$notify_msg" -t 700
fi
