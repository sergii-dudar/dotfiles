#!/usr/bin/env bash
# Tmux Window Manager (fast variant): manages windows in the current session with minimal spawns.
# - no fzf-tmux layer, no tput, no preview, pane path resolved by tmux only when a window is created
# - no rename (ctrl-r): use sesh.windows.full.fzf.sh (prefix+W) for that
# triggered by tmux (prefix+w): .tmux.conf opens the popup itself and sources this file with SESH_FAST_POPUP=1;
# the wrapper below only runs when the script is executed directly. Keep it bash- and zsh-compatible.

# popup width: 70% of the client, but never narrower than the one-line hints need (keep MIN_W in sync with .tmux.conf)
MIN_W=58 # hints fit from 56 cols (measured) + margin 2
if [ -n "$TMUX" ] && [ -z "$SESH_FAST_POPUP" ]; then
    case "$0" in /*) self="$0" ;; *) self="$PWD/$0" ;; esac
    # one tmux call: the width is a tmux format (70% of the client, at least MIN_W, at most 100%; the second threshold is
    # ceil(MIN_W / 0.7)), and the popup shell sources this file instead of executing it (one process less)
    exec tmux run-shell -C "display-popup -E -B -w #{?#{e|<:#{client_width},$MIN_W},100%,#{?#{e|<:#{client_width},$(( (MIN_W * 10 + 6) / 7 ))},$MIN_W,70%}} -h 60% 'SESH_FAST_POPUP=1; . $self'"
fi

b=$'\033[1m'
n=$'\033[0m'
s=$'\033[35m'
k=$'\033[32m'  # green
sep="${s}${n}"

iwin=$'\033[38;5;4m󱂬\033[0m'
# action icons
inew=$'\033[38;5;32m\033[0m'
inewswitch=$'\033[38;5;32m 󰿄\033[0m'
ikill=$'\033[38;5;1m\033[0m'

LIST_CMD='tmux list-windows -F "#{window_index}: #{window_name}"'
# new windows start in the current pane's directory: tmux expands the format in -c itself, no display-message call at start
NEW_WIN='tmux new-window -c "#{pane_current_path}" -n'

window="$(
    eval "$LIST_CMD" | fzf --height 100% --border --border-label " Tmux Window Manager " --no-sort --ansi \
        --prompt "${iwin} Windows: " \
        --header "[${b}${k}󰘴n${n}]:New ${inew}  ${sep} [${b}${k}󰘴g${n}]:New & Switch ${inewswitch}  ${sep} [${b}${k}󰘴d${n}]:Kill ${ikill}" \
        --bind 'tab:down,btab:up' \
        --bind 'ctrl-n:execute(name={q}; [ -z "$name" ] && printf "Window name: " && read name; [ -n "$name" ] && '"$NEW_WIN"' "$name")+clear-query+reload('"$LIST_CMD"')' \
        --bind 'ctrl-g:execute(name={q}; [ -z "$name" ] && printf "Window name: " && read name; [ -n "$name" ] && '"$NEW_WIN"' "$name")+abort' \
        --bind 'ctrl-d:execute(n={}; tmux kill-window -t ":${n%%:*}")+reload('"$LIST_CMD"')'
)"

[ -n "$window" ] && exec tmux select-window -t ":${window%%:*}"
exit 0 # nothing picked (esc / ctrl-c): leave quietly, otherwise tmux reports "returned 1"
