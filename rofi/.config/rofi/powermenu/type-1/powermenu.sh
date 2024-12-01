#!/usr/bin/env bash

## Author : Aditya Shakya (adi1090x)
## Github : @adi1090x
#
## Rofi   : Power Menu
#
## Available Styles
#
## style-1   style-2   style-3   style-4   style-5

# Current Theme
dir="$HOME/.config/rofi/powermenu/type-1"
theme='style-1'

# CMDs
uptime="`uptime -p | sed -e 's/up //g'`"
host=`hostname`

# Options
shutdown=' Shutdown'
reboot=' Reboot'
lock=' Lock'
suspend=' Suspend'
logout=' Logout'
yes=' Yes'
no=' No'

# Rofi CMD
rofi_cmd() {
    rofi -dmenu \
        -p "$host" \
        -mesg "Uptime: $uptime" \
        -theme ${dir}/${theme}.rasi
}

# Confirmation CMD
confirm_cmd() {
    rofi -theme-str 'window {location: center; anchor: center; fullscreen: false; width: 250px;}' \
        -theme-str 'mainbox {children: [ "message", "listview" ];}' \
        -theme-str 'listview {columns: 2; lines: 1;}' \
        -theme-str 'element-text {horizontal-align: 0.5;}' \
        -theme-str 'textbox {horizontal-align: 0.5;}' \
        -dmenu \
        -p 'Confirmation' \
        -mesg 'Are you Sure?' \
        -theme ${dir}/${theme}.rasi
}

# Ask for confirmation
confirm_exit() {
    echo -e "$yes\n$no" | confirm_cmd
}

# Pass variables to rofi dmenu
run_rofi() {
    echo -e "$lock\n$logout\n$reboot\n$shutdown\n$suspend" | rofi_cmd
}

# Execute Command
run_cmd() {
    selected="$(confirm_exit)"
    if [[ "$selected" == "$yes" ]]; then
        if [[ $1 == '--shutdown' ]]; then
            systemctl poweroff
        elif [[ $1 == '--reboot' ]]; then
            systemctl reboot
        elif [[ $1 == '--suspend' ]]; then
            # mpc -q pause
            # amixer set Master mute
            systemctl suspend
        elif [[ $1 == '--logout' ]]; then
            pkill -KILL -u "$USER"
        fi
    else
        exit 0
    fi
}

exec_on_exit_action() {
    ~/dotfiles/bin/wmscripts/on_logout_action.sh
}

# Actions
chosen="$(run_rofi)"
case ${chosen} in
    $shutdown)
        #run_cmd --shutdown
        exec_on_exit_action
        systemctl poweroff
        ;;
    $reboot)
        #run_cmd --reboot
        exec_on_exit_action
        systemctl reboot
        ;;
    $lock)
        sh "$HOME/dotfiles/bin/screen-lock"
        ;;
    $suspend)
        #run_cmd --suspend
        exec_on_exit_action
        systemctl suspend
        ;;
    $logout)
        #run_cmd --logout
        exec_on_exit_action
        pkill -KILL -u "$USER"
        ;;
esac