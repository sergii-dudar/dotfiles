#!/usr/bin/env bash
# Tmux Window Manager (fast variant): manages windows in the current session with minimal spawns.
# - no fzf-tmux layer, no tput, preview hidden until ctrl-p
# - no rename (ctrl-r): use sesh.windows.full.fzf.sh (prefix+W) for that
# triggered by tmux (prefix+w)

if [ -n "$TMUX" ] && [ -z "$SESH_FAST_POPUP" ]; then
case "$0" in /*) self="$0" ;; *) self="$PWD/$0" ;; esac
    exec tmux display-popup -E -B -w 40% -h 60% "SESH_FAST_POPUP=1 '$self'"
fi

b=$'\033[1m'
n=$'\033[0m'
s=$'\033[35m'
k=$'\033[32m'  # green
sep="${s}${n}"

iwin=$'\033[38;5;4m󱂬\033[0m'
inew=$'\033[38;5;32m\033[0m'
inewswitch=$'\033[38;5;32m 󰿄\033[0m'
ikill=$'\033[38;5;1m\033[0m'
ipreview=$'\033[38;5;245m󰈈\033[0m'

LIST_CMD='tmux list-windows -F "#{window_index}: #{window_name}"'
PANE_PATH=$(tmux display-message -p '#{pane_current_path}')

window="$(
    eval "$LIST_CMD" | fzf --height 100% --border --no-sort --ansi \
        --border-label " Tmux Window Manager " --prompt "${iwin} Windows: " \
        --header "[${b}${k}󰘴n${n}]:New ${inew} ${sep} [${b}${k}󰘴g${n}]:New & Switch ${inewswitch}"$'\n'"[${b}${k}󰘴d${n}]:Kill ${ikill} ${sep} [${b}${k}󰘴p${n}]:Preview ${ipreview}" \
        --bind 'tab:down,btab:up' \
        --bind 'ctrl-n:execute(name={q}; [ -z "$name" ] && printf "Window name: " && read name; [ -n "$name" ] && tmux new-window -n "$name" -c '"$PANE_PATH"')+clear-query+reload('"$LIST_CMD"')' \
        --bind 'ctrl-g:execute(name={q}; [ -z "$name" ] && printf "Window name: " && read name; [ -n "$name" ] && tmux new-window -n "$name" -c '"$PANE_PATH"')+abort' \
        --bind 'ctrl-d:execute(n={}; tmux kill-window -t ":${n%%:*}")+reload('"$LIST_CMD"')' \
        --bind 'ctrl-p:toggle-preview' \
        --preview 'n={}; tmux capture-pane -e -p -t ":${n%%:*}"' \
        --preview-window=down,65%,hidden
)"

[ -n "$window" ] && exec tmux select-window -t ":${window%%:*}"