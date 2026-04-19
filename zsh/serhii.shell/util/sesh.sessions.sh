# shares sesh script that using by zsh (alt-s) and tmux (prefix+t)
b=$(tput bold)
n=$(tput sgr0)

s=$'\033[35m'
k=$'\033[32m'  # green
bl=$'\033[34m' # blue
y=$'\033[33m'  # yellow

sep="${s}${n}"

itmux=$'\033[38;5;4m\033[0m'
izoxide=$'\033[38;5;14m󱓞\033[0m'
isearch=$'\033[38;5;141m\033[0m'
iconfigured=$'\033[38;5;110m\033[0m'

inew=$'\033[38;5;32m\033[0m'
ikill=$'\033[38;5;1m\033[0m'

OS_TYPE=$(uname)
function isMacOs() {
    if [[ "$OS_TYPE" == "Darwin" ]]; then
        #echo "current is MacOs..."
        return 0 # true
    else
        #echo "current is Linux..."
        return 1 # false
    fi
}

if isMacOs; then
    session="$(
        sesh list -t -i | fzf-tmux -x 100 -y 100 -p 100%,90% --height 90% \
            --no-sort --ansi --border-label " Tmux Session Manager " --prompt "${itmux} Tmux: " \
            --header "[${b}${k}󰘴a${n}]:All ${y}⚡${n} ${sep} [${b}${k}󰘴t${n}]:Tmux ${bl}${n}  ${sep} [${b}${k}󰘴x${n}]:Z ${izoxide} ${sep} [${b}${k}󰘴g${n}]:Conf ${iconfigured}  ${sep} [${b}${k}󰘴f${n}]:Find ${isearch}  ${sep} [${b}${k}󰘴n${n}]:New ${inew}  ${sep} [${b}${k}󰘴d${n}]:Kill ${ikill}" \
            --bind 'tab:down,btab:up' \
            --bind "ctrl-a:change-prompt(${y}⚡${n} Sesh All: )+reload(sesh list -i)" \
            --bind "ctrl-t:change-prompt(${itmux} Tmux: )+reload(sesh list -t -i)" \
            --bind "ctrl-x:change-prompt(${izoxide} Zoxide: )+reload(sesh list -z -i)" \
            --bind "ctrl-g:change-prompt(${iconfigured}  Conf: )+reload(sesh list -c -i)" \
            --bind 'ctrl-f:change-prompt(  Find: )+reload((fd -H -d 5 -t d -E ".*" -E "*books*" . ~ | xargs -I folder printf " folder\n" ; \
                                                      fd -H -d 3 -t d -p ~/.tmux ~ | xargs -I folder printf " folder\n" ; \
                                                      fd -H -d 2 -t d -p ~/.config ~ | xargs -I folder printf " folder\n" ; \
                                                      fd -H -d 5 -t d -p ~/.local/share/nvim/lazy ~ | xargs -I folder printf " folder\n"))' \
            --bind 'ctrl-d:execute(tmux has-session -t $(echo {} | cut -c3-) 2>/dev/null && tmux kill-session -t $(echo {} | cut -c3-))+change-prompt( Tmux Kill: )+reload(sesh list -t -i)' \
            --bind 'ctrl-n:execute([ -n "{q}" ] && (tmux has-session -t {q} 2>/dev/null || tmux new-session -s {q} -n {q} -d))+change-prompt(  Tmux New: )+reload(sesh list -t -i)' \
            --preview 'tmux has-session -t $(echo {} | cut -c3-) 2>/dev/null && tmux capture-pane -peJt $(echo {} | cut -c3-) || eza --tree --icons --level=1 --color=always --group-directories-first "$(eval echo $(echo {} | cut -c3-))"' \
            --preview-window=down,75%
    )"

    zle reset-prompt > /dev/null 2>&1 || true
    [[ -n "$session" ]] && sesh connect "$session"
    #echo "$session"

else

    session="$(
        sesh list -t -i | fzf-tmux -x 100 -y 100 -p 100%,90% --height 90% \
            --no-sort --ansi --border-label " Tmux Session Manager " --prompt "${bl}${n} Tmux: " \
            --header "[${b}${k}󰘴a${n}]:All ${y}⚡${n} ${sep} [${b}${k}󰘴t${n}]:Tmux ${itmux} ${sep} [${b}${k}󰘴x${n}]:Z ${izoxide} ${sep} [${b}${k}󰘴g${n}]:Conf ${iconfigured}  ${sep} [${b}${k}󰘴f${n}]:Find ${isearch}  ${sep} [${b}${k}󰘴n${n}]:New ${inew}  ${sep} [${b}${k}󰘴d${n}]:Kill ${ikill}" \
            --bind 'tab:down,btab:up' \
            --bind "ctrl-a:change-prompt(${y}⚡${n} Sesh All: )+reload(sesh list -i)" \
            --bind "ctrl-t:change-prompt(${itmux} Tmux: )+reload(sesh list -t -i)" \
            --bind "ctrl-x:change-prompt(${izoxide} Zoxide: )+reload(sesh list -z -i)" \
            --bind "ctrl-g:change-prompt(${iconfigured}  Conf: )+reload(sesh list -c -i)" \
            --bind 'ctrl-f:change-prompt(  Find: )+reload((fd -H -d 5 -t d -E ".*" -E "*books*" . ~ | xargs -I folder printf "\e[34m\e[0m folder\n" ; \
                                                      fd -H -d 3 -t d -p ~/.tmux ~ | xargs -I folder printf "\e[34m\e[0m folder\n" ; \
                                                      fd -H -d 2 -t d -p ~/.config ~ | xargs -I folder printf "\e[34m\e[0m folder\n" ; \
                                                      fd -H -d 5 -t d -p ~/.local/share/nvim/lazy ~ | xargs -I folder printf "\e[34m\e[0m folder\n"))' \
            --bind 'ctrl-d:execute(tmux has-session -t $(echo {} | cut -c4-) 2>/dev/null && tmux kill-session -t $(echo {} | cut -c4-))+change-prompt( Tmux Kill: )+reload(sesh list -t -i)' \
            --bind 'ctrl-n:execute([ -n "{q}" ] && (tmux has-session -t {q} 2>/dev/null || tmux new-session -s {q} -n {q} -d))+change-prompt( Tmux New: )+reload(sesh list -t -i)' \
            --preview 'tmux has-session -t $(echo {} | cut -c4-) 2>/dev/null && tmux capture-pane -peJt $(echo {} | cut -c4-) || eza --tree --icons --level=1 --color=always --group-directories-first "$(eval echo $(echo {} | cut -c4-))"' \
            --preview-window=down,75%
    )"

    zle reset-prompt > /dev/null 2>&1 || true
    [[ -n "$session" ]] && sesh connect "$session"

fi