#!/usr/bin/env bash
# Tmux Window Manager (fast variant): manages windows in the current session with minimal spawns.
# - no fzf-tmux layer, no tput, no preview
# - no rename (ctrl-r): use sesh.windows.full.fzf.sh (prefix+W) for that
# triggered by tmux (prefix+w)

# popup width: 70% of the client, but never narrower than the one-line hints need
MIN_W=54 # hints fit from 52 cols (measured) + margin 2
if [ -n "$TMUX" ] && [ -z "$SESH_FAST_POPUP" ]; then
    case "$0" in /*) self="$0" ;; *) self="$PWD/$0" ;; esac
    cw=$(tmux display-message -p '#{client_width}'); cw=${cw:-200}; w=$((cw * 70 / 100))
    [ "$w" -lt "$MIN_W" ] && w=$MIN_W; [ "$w" -gt "$cw" ] && w=$cw
    exec tmux display-popup -E -B -w "$w" -h 60% "SESH_FAST_POPUP=1 '$self'"
fi

b=$'\033[1m'
n=$'\033[0m'
s=$'\033[35m'
k=$'\033[32m'  # green
sep="${s}${n}"

iwin=$'\033[38;5;4m󱂬\033[0m'
# action icons
inew=$'\033[38;5;32m\033[0m'
inewswitch=$'\033[38;5;32m 󰿄\033[0m'
ikill=$'\033[38;5;1m\033[0m'

LIST_CMD='tmux list-windows -F "#{window_index}: #{window_name}"'
PANE_PATH=$(tmux display-message -p '#{pane_current_path}')

window="$(
    eval "$LIST_CMD" | fzf --height 100% --border --border-label " Tmux Window Manager " --no-sort --ansi \
        --prompt "${iwin} Windows: " \
        --header "[${b}${k}󰘴n${n}]:New ${inew}  ${sep} [${b}${k}󰘴g${n}]:New & Switch ${inewswitch}  ${sep} [${b}${k}󰘴d${n}]:Kill ${ikill}" \
        --bind 'tab:down,btab:up' \
        --bind 'ctrl-n:execute(name={q}; [ -z "$name" ] && printf "Window name: " && read name; [ -n "$name" ] && tmux new-window -n "$name" -c '"$PANE_PATH"')+clear-query+reload('"$LIST_CMD"')' \
        --bind 'ctrl-g:execute(name={q}; [ -z "$name" ] && printf "Window name: " && read name; [ -n "$name" ] && tmux new-window -n "$name" -c '"$PANE_PATH"')+abort' \
        --bind 'ctrl-d:execute(n={}; tmux kill-window -t ":${n%%:*}")+reload('"$LIST_CMD"')'
)"

[ -n "$window" ] && exec tmux select-window -t ":${window%%:*}"
exit 0 # nothing picked (esc / ctrl-c): leave quietly, otherwise tmux reports "returned 1"
