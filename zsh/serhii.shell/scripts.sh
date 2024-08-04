# Define ANSI escape codes for colors
BLACK='\033[0;30m'
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
BLUE='\033[0;34m'
MAGENTA='\033[0;35m'
CYAN='\033[0;36m'
BOLD='\033[1m'
BOLD_BLUE='\033[1;34m'
BOLD_YELLOW='\033[1;33m'
UNDERLINE='\033[4m'
RESET='\033[0m' # Reset color to default

#TGREEN='%F{green}'
#TRESET='%f'  # Reset color to default
#NL='
#'
#PS1="${PS1}${NL}${TGREEN}> ${TRESET}"

OS_TYPE=$(uname)
function isMacOs() {
    if [[ "$OS_TYPE" == "Darwin" ]]; then
        #echo "current is MacOs..."
        return 0  # true
    else
        #echo "current is Linux..."
        return 1  # false
    fi
}

function getLogLevel() {
    if [[ "$1" == "--help" || "$1" == "-h" ]]; then
        cat << EOF
getLogLevel [PORT1,PORT2...] [package]
EOF
        return 0
    fi

    ports=(${(@s:,:)1})
    for port in "${ports[@]}"; do
            curl --silent --request GET \
                 --url "http://localhost:$port/actuator/loggers/$2"
    done
}

function changeLogLevel() {
    if [[ "$1" == "--help" || "$1" == "-h" ]]; then
        cat << EOF
changeLogLevel [PORT1,PORT2...] [package] [LOG LEVEL]
EOF
        return 0
    fi

    ports=(${(@s:,:)1})
    for port in "${ports[@]}"; do

            curl --request POST \
                --url "http://localhost:$port/actuator/loggers/$2" \
                --header 'Content-Type: application/json' \
                --data "{
                \"configuredLevel\": \"$3\"
                }"
            echo -e ">>>> $port: Level to package: ${BOLD_BLUE}\"$2\"${RESET} successfully change to ${BOLD_YELLOW}\"$3\"${RESET}"
            echo -e ">>>> $port: Actual level of package: ${BOLD_BLUE}\"$2\"${RESET} is: ${BOLD_YELLOW}$(getLogLevel $port "$2")${RESET}"
    done
}

function getLogLevel8091() {
    if [[ "$1" == "--help" || "$1" == "-h" ]]; then
        cat << EOF
getLogLevel8091 [package]
EOF
        return 0
    fi

    getLogLevel 8091 "$1"
}

function changeLogLevel8091() {
    if [[ "$1" == "--help" || "$1" == "-h" ]]; then
        cat << EOF
changeLogLevel8091 [package] [LOG LEVEL]
EOF
        return 0
    fi

    changeLogLevel 8091 "$1" "$2"
}

function chrome() {
	google-chrome > /dev/null 2>&1 &
}

function idea() {
    if isMacOs; then
        '/Applications/IntelliJ IDEA.app/Contents/MacOS/idea'
    else
        intellij-idea-ultimate "$1" > /dev/null 2>&1 &
    fi
}

function open_terminal() {
    alacritty
}

function files() {
    if isMacOs; then
        if [ -z "$1" ]; then
            open .
        else
            open -R "$1"
        fi
    else
        nautilus "$1" > /dev/null 2>&1 &
    fi
}

function s_restart() {
    sudo systemctl restart "$1"
}

function s_status() {
    sudo systemctl status "$1"
}

function copy_content() {
    if isMacOs; then
        pbcopy < $1
    else
        xclip -selection clipboard $1
    fi
}

function topCommands() {
    history | awk 'BEGIN {FS="[ \t]+|\\|"} {print $3}' | sort | uniq -c | sort -nr | head -$1
}

function copy_file() {
    if isMacOs; then
        # brew install findutils
        # brew install coreutils
        osascript -e{'on run{a}','set the clipboard to posix file a',end} "$(greadlink -f -- "$1")"
    else
        # on linux no such way, we just copying content to clipboard
        xclip -selection clipboard $1
    fi
}

function mkcd () {
    mkdir -p -- "$1" && cd -p -- "$1"
}

function tmux_popup() {
    tmux display-popup -E sh -c tmux attach -t popup_terminal || (tmux new-session -d -s popup_terminal -c "$PWD" && tmux attach -t popup_terminal)
}

function help() {
    $1 --help | nvim -R
}

function find_git_root() {
    local home="$HOME"
    local dir="$PWD"

    while [ "$dir" != "$home" ]; do
        #echo "$dir"
        found_files=$(fd -d 1 -t d -H "\.git\b" "$dir")
        if [[ -n $found_files ]]; then
            return 0
        fi
        dir=$(dirname "$dir")
    done

    return 1
}

ranger() {
    tmp="$(mktemp)"
    command ranger --choosedir="$tmp" "$@"
    if [ -f "$tmp" ]; then
        dir="$(cat "$tmp")"
        rm -f "$tmp"
        if [ -d "$dir" ]; then
            if [ "$dir" != "$(pwd)" ]; then
                builtin cd "$dir" || return
            fi
        fi
    fi
}
function yy() {
	local tmp="$(mktemp -t "yazi-cwd.XXXXXX")"
	yazi "$@" --cwd-file="$tmp"
	if cwd="$(cat -- "$tmp")" && [ -n "$cwd" ] && [ "$cwd" != "$PWD" ]; then
		builtin cd -- "$cwd" || return
	fi
	rm -f -- "$tmp"
}