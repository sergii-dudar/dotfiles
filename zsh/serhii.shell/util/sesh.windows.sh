# Tmux Window Manager - manages windows in current tmux session
# triggered by tmux (prefix+w)
b=$(tput bold)
n=$(tput sgr0)

s=$'\033[35m'
k=$'\033[32m'  # green
bl=$'\033[34m' # blue
y=$'\033[33m'  # yellow

sep="${s}${n}"

iwin=$'\033[38;5;4m\033[0m'

inew=$'\033[38;5;32m\033[0m'
ikill=$'\033[38;5;1m\033[0m'
irename=$'\033[38;5;214m󰏫\033[0m'

window="$(
    sesh window | fzf-tmux -x 100 -y 100 -p 100%,90% --height 90% \
        --no-sort --ansi --border-label " Tmux Window Manager " --prompt "${iwin} Windows: " \
        --header "[${b}${k}󰘴n${n}]:New ${inew}  ${sep} [${b}${k}󰘴d${n}]:Kill ${ikill}  ${sep} [${b}${k}󰘴r${n}]:Rename ${irename}" \
        --bind 'tab:down,btab:up' \
        --bind 'ctrl-n:execute([ -n "{q}" ] && tmux new-window -n "{q}")+clear-query+reload(sesh window)' \
        --bind 'ctrl-d:execute(tmux kill-window -t :$(tmux list-windows -F "#{window_name} #{window_index}" | grep "^{} " | cut -d" " -f2))+reload(sesh window)' \
        --bind 'ctrl-r:execute([ -n "{q}" ] && tmux rename-window -t :$(tmux list-windows -F "#{window_name} #{window_index}" | grep "^{} " | cut -d" " -f2) "{q}")+clear-query+reload(sesh window)' \
        --preview 'tmux capture-pane -e -p -t :$(tmux list-windows -F "#{window_name} #{window_index}" | grep "^{} " | cut -d" " -f2)' \
        --preview-window=down,75%
)"

zle reset-prompt > /dev/null 2>&1 || true
if [[ -n "$window" ]]; then
    win_idx=$(tmux list-windows -F "#{window_name} #{window_index}" | grep "^${window} " | cut -d" " -f2)
    tmux select-window -t ":${win_idx}"
fi
