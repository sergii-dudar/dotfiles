APP="$1"

case "$APP" in
    "yazi")
        class="com.scratchpad.yazi"
        cmd="ghostty --class=com.scratchpad.yazi -e ~/.cargo/bin/yazi"
        notify="📂 Yazi Manager"
        ;;
    "music")
        class="com.scratchpad.music"
        cmd="ghostty --class=com.scratchpad.music -e rmpc"
        notify="🎹 MPD-RMPC"
        ;;
    "nautilus")
        class="org.gnome.Nautilus"
        cmd="nautilus"
        notify="📂 Nautilus"
        ;;
    "telegram")
        class="org.telegram.desktop"
        cmd="telegram-desktop"
        notify="💬 Telegram"
        ;;
    "youtube_music")
        class="brave-cinhimbnkkaeohfgghhklpknlkffjgod-Default"
        cmd="brave --profile-directory=Default --app-id=cinhimbnkkaeohfgghhklpknlkffjgod"
        notify="💽 Music"
        ;;
    "google_chat")
        class="brave-mdpkiolbdkhdjpekfbkbmhigcaggjagi-Default"
        cmd="brave --profile-directory=Default --app-id=mdpkiolbdkhdjpekfbkbmhigcaggjagi"
        notify="✉️ Google Chat"
        ;;
    "monkey_type")
        class="brave-picebhhlijnlefeleilfbanaghjlkkna-Default"
        cmd="brave --profile-directory=Default --app-id=picebhhlijnlefeleilfbanaghjlkkna"
        notify="  Monkey Type"
        ;;
    *)
        echo "$APP - unsupported"
        exit 1
        ;;
esac

function is_app_running() {
    hyprctl clients -j | jq -e --arg cls "$class" \
        'map(select(.class == $cls)) | length > 0'
}

function get_window_address() {
    addr=$(hyprctl clients -j | jq -r --arg cls "$class" \
        'map(select(.class == $cls)) | .[0].address // empty')

    if [ -z "$addr" ]; then
        # hyprctl notify [ICON] [TIME_MS] [COLOR] [MESSAGE]
        # hyprctl notify 0 10000 "rgb(ff1ea3)" "Failed to find window address by class: $class"
        hyprctl seterror "rgba(66ee66ff)" "$0: Failed to find window address by class: $class"
        exit 1
    fi

    echo "$addr"
}

function is_app_visible() {
    hyprctl clients -j | jq -e --arg cls "$class" \
        'map(select(.class == $cls and .workspace.name != "special:scratchpad")) | length > 0'
}

function move_app_to_scratchpad() {
    addr=$(get_window_address)
    [ -n "$addr" ] && hyprctl dispatch movetoworkspacesilent special:scratchpad,address:"$addr"
}

function move_app_to_current_workspace_and_focus() {
    addr=$(get_window_address)
    current_ws=$(hyprctl activeworkspace -j | jq -r .name)
    # [ -n "$addr" ] && \
        #     hyprctl dispatch movetoworkspacesilent "$current_ws", address:"$addr" && \
        #     hyprctl dispatch focuswindow address:"$addr" && \
        #     hyprctl dispatch centerwindow

    hyprctl --batch "\
        dispatch movetoworkspacesilent $current_ws, address:$addr ;\
        dispatch focuswindow address:$addr ;\
        dispatch centerwindow"
}

# Start if not running
if ! is_app_running; then
    bash -c "$cmd" >/dev/null 2>&1 &
    for i in {1..20}; do
        sleep 0.25
        is_app_running && break
    done
    # move_app_to_scratchpad # declared by hypr roles
fi

function hide_all_other_scratchpads() {
    hyprctl clients -j | jq -r '.[] |
        select(.workspace.name != "special:scratchpad") |
        select(.tags[]? | test("^scratchpad")) |
        select(.class != "'"$class"'") |
    .address' | while read -r addr; do
        hyprctl dispatch movetoworkspacesilent special:scratchpad, address:"$addr"
    done
}

if is_app_visible; then
    move_app_to_scratchpad
else
    hide_all_other_scratchpads
    move_app_to_current_workspace_and_focus
    notify-send "$notify" -t 700
fi

# notify waybar `custom/hypr-scratchpad` module to update
pkill -RTMIN+3 waybar