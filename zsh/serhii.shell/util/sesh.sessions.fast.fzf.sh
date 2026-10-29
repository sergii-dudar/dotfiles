#!/usr/bin/env bash
# Tmux Session Manager (fast variant): minimal process spawns.
# - no fzf-tmux layer: inside tmux it re-runs itself directly in a tmux popup
# - no tput calls: escape sequences inlined
# - preview hidden until toggled with ctrl-p, so cursor moves spawn nothing
# - no find (ctrl-f) / rename (ctrl-r): use sesh.sessions.full.fzf.sh (prefix+T) for those
# Used by tmux (prefix+t) and zsh (alt-s / ctrl-f widget in tmux.scripts.sh).

if [ -n "$TMUX" ] && [ -z "$SESH_FAST_POPUP" ]; then
    case "$0" in /*) self="$0" ;; *) self="$PWD/$0" ;; esac
    exec tmux display-popup -E -B -w 30% -h 60% "SESH_FAST_POPUP=1 '$self'"
fi

b=$'\033[1m'
n=$'\033[0m'
s=$'\033[35m'
k=$'\033[32m'  # green
bl=$'\033[34m' # blue
y=$'\033[33m'  # yellow
sep="${s}${n}"

itmux=$'\033[38;5;4m\033[0m'
izoxide=$'\033[38;5;14m󱓞\033[0m'
iconfigured=$'\033[38;5;110m\033[0m'
inew=$'\033[38;5;32m\033[0m'
ikill=$'\033[38;5;1m\033[0m'
ipreview=$'\033[38;5;245m󰈈\033[0m'

session="$(
    sesh list -t -i | fzf --height 100% --border --no-sort --ansi \
        --border-label " Tmux Session Manager " --prompt "${itmux} Tmux: " \
        --header "[${b}${k}󰘴a${n}]:All ${y}⚡${n} ${sep} [${b}${k}󰘴t${n}]:Tmux ${bl}${n} ${sep} [${b}${k}󰘴x${n}]:Z ${izoxide} ${sep} [${b}${k}󰘴g${n}]:Conf ${iconfigured}"$'\n'"[${b}${k}󰘴n${n}]:New ${inew} ${sep} [${b}${k}󰘴d${n}]:Kill ${ikill} ${sep} [${b}${k}󰘴p${n}]:Preview ${ipreview}" \
        --bind 'tab:down,btab:up' \
        --bind "ctrl-a:change-prompt(${y}⚡${n} Sesh All: )+reload(sesh list -i)" \
        --bind "ctrl-t:change-prompt(${itmux} Tmux: )+reload(sesh list -t -i)" \
        --bind "ctrl-x:change-prompt(${izoxide} Zoxide: )+reload(sesh list -z -i)" \
        --bind "ctrl-g:change-prompt(${iconfigured}  Conf: )+reload(sesh list -c -i)" \
        --bind 'ctrl-d:execute(n={}; tmux kill-session -t "${n#??}" 2>/dev/null)+change-prompt( Tmux Kill: )+reload(sesh list -t -i)' \
        --bind 'ctrl-n:execute([ -n {q} ] && tmux new-session -d -s {q} -n {q} 2>/dev/null)+change-prompt(  Tmux New: )+reload(sesh list -t -i)' \
        --bind 'ctrl-p:toggle-preview' \
        --preview 'n={}; n=${n#??}; tmux capture-pane -peJt "$n" 2>/dev/null || eza --tree --icons --level=1 --color=always --group-directories-first "$(eval echo "$n")"' \
        --preview-window=down,65%,hidden
)"

[ -n "$session" ] && exec sesh connect "$session"
