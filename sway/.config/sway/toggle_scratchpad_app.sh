# if ! pgrep --full "$run_cmd" >/dev/null; then
# fi

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

if [ $shell = "xwayland" ]; then
    running_grep_math="$instance_name"
    jq_wlprop_selector=".window_properties.class? == \"$class_name\" and .window_properties.instance? == \"$instance_name\""
    # jq_wlprop_selector_not=".window_properties.class? != \"$class_name\" and .window_properties.instance? != \"$instance_name\""
    jq_wlprop_selector_not=".window_properties.instance? != \"$instance_name\""
    swaymsg_selector="[class=$class_name instance=$instance_name]"
else
    running_grep_math="$app_id"
    jq_wlprop_selector=".app_id? == \"$app_id\""
    jq_wlprop_selector_not=".app_id? != \"$app_id\""
    swaymsg_selector="[app_id=$app_id]"
fi

function is_app_running() {
    swaymsg -t get_tree | grep -q "$running_grep_math"
}

# Helper: check if target app window is currently visible
function is_target_visible() {
    swaymsg -t get_tree | jq -e '.. | objects |
        select('"$jq_wlprop_selector"') |
    select(.scratchpad_state == "fresh" and .visible == true)' > /dev/null
}

# Helper: hide all other scratchpad windows except the target
function hide_all_other_scratchpads() {
    swaymsg -t get_tree |
    jq -r '.. | objects |
            select(.scratchpad_state? == "fresh" and .visible == true) |
            select('"$jq_wlprop_selector_not"') |
    .id' |
    while read -r win_id; do
        swaymsg "[con_id=$win_id]" move to scratchpad
    done
}

if ! is_app_running; then
    bash -c "$run_cmd" > /dev/null 2>&1 &

    # Wait until the window is available to sway
    for i in {1..20}; do
        is_app_running && break
        sleep 0.25
    done
fi

if swaymsg -t get_tree | jq -e '.. | objects | select('"$jq_wlprop_selector"') | select(.scratchpad_state == "none")' >/dev/null; then
    # move to scratchpad if not there yet
    swaymsg "$swaymsg_selector" floating enable, move to scratchpad
    sleep 0.5
fi

if is_target_visible; then # if pressed same scratch app button to just hite, not show anothe scratch app
    # Toggle off: hide it
    swaymsg "$swaymsg_selector" move to scratchpad
    # notify-send "$notify_msg" -t 700
else
    # Toggle on: hide others, then show target
    hide_all_other_scratchpads
    swaymsg "$swaymsg_selector" scratchpad show, border pixel 5, resize set 75ppt 80ppt, move position center
    notify-send "$notify_msg" -t 700
fi
