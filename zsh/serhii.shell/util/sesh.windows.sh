# Tmux Window Manager - manages windows in current tmux session
# triggered by tmux (prefix+w)
b=$(tput bold)
n=$(tput sgr0)

s=$'\033[35m'
k=$'\033[32m'  # green
bl=$'\033[34m' # blue
y=$'\033[33m'  # yellow

sep="${s}${n}"

iwin=$'\033[38;5;4m󱂬\033[0m'

inew=$'\033[38;5;32m\033[0m'
inewswitch=$'\033[38;5;32m 󰿄\033[0m'
ikill=$'\033[38;5;1m\033[0m'
irename=$'\033[38;5;214m󰑕\033[0m'

LIST_CMD='tmux list-windows -F "#{window_index}: #{window_name}"'

window="$(
    eval "$LIST_CMD" | fzf-tmux -x 100 -y 100 -p 100%,90% --height 90% \
        --no-sort --ansi --border-label " Tmux Window Manager " --prompt "${iwin} Windows: " \
        --header "[${b}${k}󰘴n${n}]:New ${inew}  ${sep} [${b}${k}󰘴m${n}]:New & Switch ${inewswitch}  ${sep} [${b}${k}󰘴d${n}]:Kill ${ikill}  ${sep} [${b}${k}󰘴r${n}]:Rename ${irename}" \
        --bind 'tab:down,btab:up' \
        --bind 'ctrl-n:execute(name={q}; [ -z "$name" ] && printf "Window name: " && read name; [ -n "$name" ] && tmux new-window -n "$name")+clear-query+reload('"$LIST_CMD"')' \
        --bind 'ctrl-m:execute(name={q}; [ -z "$name" ] && printf "Window name: " && read name; [ -n "$name" ] && tmux new-window -n "$name")+abort' \
        --bind 'ctrl-d:execute(tmux kill-window -t :$(echo {} | cut -d: -f1))+reload('"$LIST_CMD"')' \
        --bind 'ctrl-r:execute(printf "Rename to: " && read name && [ -n "$name" ] && tmux rename-window -t :$(echo {} | cut -d: -f1) "$name")+reload('"$LIST_CMD"')' \
        --preview 'tmux capture-pane -e -p -t :$(echo {} | cut -d: -f1)' \
        --preview-window=down,75%
)"

zle reset-prompt > /dev/null 2>&1 || true
if [[ -n "$window" ]]; then
    win_idx=$(echo "$window" | cut -d: -f1)
    tmux select-window -t ":${win_idx}"
fi