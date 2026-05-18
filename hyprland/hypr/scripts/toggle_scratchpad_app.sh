#!/usr/bin/env bash

APP="$1"

case "$APP" in
    "yazi")
        class="com.scratchpad.yazi"
        # cmd="ghostty --class=com.scratchpad.yazi -e ~/.cargo/bin/yazi"
        # cmd="alacritty --class=com.scratchpad.yazi -e ~/.cargo/bin/yazi"
        cmd="foot --app-id=com.scratchpad.yazi -e ~/.cargo/bin/yazi"
        notify="📂 Yazi Manager"
        ;;
    "mini_terminal")
        class="com.scratchpad.mini_terminal"
        # cmd="ghostty --class=com.scratchpad.mini_terminal"
        # cmd="alacritty --class=com.scratchpad.mini_terminal"
        cmd="foot --app-id=com.scratchpad.mini_terminal"
        notify="🧑🏻‍💻 Mini Terminal"
        ;;
    "music")
        class="com.scratchpad.music"
        # cmd="ghostty --class=com.scratchpad.music -e rmpc"
        # cmd="alacritty --class=com.scratchpad.music -e ~/.cargo/bin/rmpc"
        cmd="foot --app-id=com.scratchpad.music -e ~/.cargo/bin/rmpc"
        notify="🎹 MPD-RMPC"
        ;;
    "nautilus")
        class="org.gnome.Nautilus"
        cmd="nautilus"
        notify="📂 Nautilus"
        ;;
    "telegram")
        class="org.telegram.desktop"
        cmd="Telegram || telegram-desktop || telegram"
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
    "vim_hero")
        class="brave-beifkklpdmlhanbkafbcldldbgnglbpn-Default"
        cmd="brave --profile-directory=Default --app-id=beifkklpdmlhanbkafbcldldbgnglbpn"
        notify="⌨️ Vim Hero"
        ;;
    *)
        echo " $APP is unsupported"
        notify="❌ $APP is unsupported"
        exit 1
        ;;
esac

function set_scratch_app_roles() {
    addr="$1"

    # Calculate 75% x 80% of focused monitor resolution
    eval "$(hyprctl monitors -j | jq -r '
        map(select(.focused == true)) | .[0] |
        "mon_w=\(.width / .scale | floor) mon_h=\((.height - .reserved[1] - .reserved[3]) / .scale | floor)"
    ')"
    win_w=$((mon_w * 75 / 100))
    win_h=$((mon_h * 80 / 100))

    hyprctl eval "
        hl.dispatch(hl.dsp.window.float({ action = 'on', window = 'address:$addr' }))
        hl.dispatch(hl.dsp.window.resize({ x = $win_w, y = $win_h, window = 'address:$addr' }))
        hl.dispatch(hl.dsp.window.center('address:$addr'))
        hl.dispatch(hl.dsp.window.set_prop({ prop = 'active_border_color', value = 'rgb(AB9DF2)', window = 'address:$addr' }))
        hl.dispatch(hl.dsp.window.set_prop({ prop = 'border_size', value = '5', window = 'address:$addr' }))
        hl.dispatch(hl.dsp.window.set_prop({ prop = 'dim_around', value = '1', window = 'address:$addr' }))
        hl.dispatch(hl.dsp.window.set_prop({ prop = 'opacity', value = '0.97', window = 'address:$addr' }))
    "
}

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

function is_app_floating() {
    local addr=$1
    hyprctl clients -j | jq -e --arg addr "$addr" 'any(.address == $addr and .floating == true)'
}

function move_app_to_scratchpad() {
    addr=$(get_window_address)
    if [ -n "$addr" ]; then
        hyprctl eval "hl.dispatch(hl.dsp.window.move({ workspace = 'special:scratchpad', window = 'address:$addr', follow = false }))"
    fi
}

function move_app_to_current_workspace_and_focus() {
    addr=$(get_window_address)
    current_ws=$(hyprctl activeworkspace -j | jq -r .name)
    # [ -n "$addr" ] && \
        #     hyprctl dispatch movetoworkspacesilent "$current_ws", address:"$addr" && \
        #     hyprctl dispatch focuswindow address:"$addr" && \
        #     hyprctl dispatch centerwindow

    # hyprctl --batch "\
        #     dispatch movetoworkspacesilent $current_ws, address:$addr ;\
        #     dispatch focuswindow address:$addr ;\
        #     dispatch tagwindow +scratchpad_dynamic address:$addr;\
        #     dispatch centerwindow"


    # in case app started not as scratchpad, make it floating scratchpad
    # hyprctl --batch "\
        #     dispatch movetoworkspacesilent $current_ws, address:$addr ;\
        #     dispatch focuswindow address:$addr ;\
        #     dispatch centerwindow"
    #
    # if ! is_app_floating "$addr"; then
    #     set_scratch_app_roles "$addr"
    # fi


    if is_app_floating "$addr"; then
        hyprctl eval "
            hl.dispatch(hl.dsp.window.move({ workspace = '$current_ws', window = 'address:$addr', follow = false }))
            hl.dispatch(hl.dsp.focus({ window = 'address:$addr' }))
            hl.dispatch(hl.dsp.window.center('address:$addr'))
        "
        set_scratch_app_roles "$addr"
    else
        # Calculate 75% x 80% of focused monitor resolution
        eval "$(hyprctl monitors -j | jq -r '
            map(select(.focused == true)) | .[0] |
            "mon_w=\(.width / .scale | floor) mon_h=\((.height - .reserved[1] - .reserved[3]) / .scale | floor)"
        ')"
        win_w=$((mon_w * 75 / 100))
        win_h=$((mon_h * 80 / 100))

        # combine to make it as smooth and fast as possible
        hyprctl eval "
            hl.dispatch(hl.dsp.window.float({ action = 'on', window = 'address:$addr' }))
            hl.dispatch(hl.dsp.window.move({ workspace = '$current_ws', window = 'address:$addr', follow = false }))
            hl.dispatch(hl.dsp.focus({ window = 'address:$addr' }))
            hl.dispatch(hl.dsp.window.resize({ x = $win_w, y = $win_h, window = 'address:$addr' }))
            hl.dispatch(hl.dsp.window.center('address:$addr'))
            hl.dispatch(hl.dsp.window.set_prop({ prop = 'active_border_color', value = 'rgb(AB9DF2)', window = 'address:$addr' }))
            hl.dispatch(hl.dsp.window.set_prop({ prop = 'border_size', value = '5', window = 'address:$addr' }))
            hl.dispatch(hl.dsp.window.set_prop({ prop = 'opacity', value = '0.97', window = 'address:$addr' }))
        "
    fi
}

function hide_all_other_scratchpads() {
    hyprctl clients -j | jq -r '.[] |
        select(.workspace.name != "special:scratchpad") |
        select(.tags[]? | test("^scratchpad")) |
        select(.class != "'"$class"'") |
    .address' | while read -r addr; do
        hyprctl eval "hl.dispatch(hl.dsp.window.move({ workspace = 'special:scratchpad', window = 'address:$addr', follow = false }))"
    done
}

# Start if not running
if is_app_running; then
    if is_app_visible; then
        move_app_to_scratchpad
    else
        hide_all_other_scratchpads
        move_app_to_current_workspace_and_focus
        notify-send "$notify" -t 700
    fi
else
    bash -c "$cmd" >/dev/null 2>&1 &
    for i in {1..20}; do
        sleep 0.25
        is_app_running && break
    done
    # move_app_to_scratchpad # declared by hypr roles

    addr=$(get_window_address)
    set_scratch_app_roles "$addr"
    hide_all_other_scratchpads
    # move_app_to_current_workspace_and_focus
    notify-send "$notify" -t 700
fi

# signal waybar `custom/hypr-scratchpad` module to update
pkill -RTMIN+3 waybar
