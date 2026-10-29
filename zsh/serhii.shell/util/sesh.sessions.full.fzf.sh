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
irename=$'\033[38;5;214m󰑕\033[0m'

# OS_TYPE=$(uname)
# function isMacOs() {
#     if [[ "$OS_TYPE" == "Darwin" ]]; then
#         #echo "current is MacOs..."
#         return 0 # true
#     else
#         #echo "current is Linux..."
#         return 1 # false
#     fi
# }
#
# if isMacOs; then
# Row format: "<1-9> <icon> <name>" for the first nine rows, "  <icon> <name>" after that.
# Strip 4 chars (cut -c5- / ${n#????}) to get the bare name; sesh connect strips the icon itself, so it gets ${session#??}.
NUM="awk 'NR<=9{print NR\" \"\$0;next}{print \"  \"\$0}'"
DIGITS='1,2,3,4,5,6,7,8,9'
# digits jump by default: a digit picks that row if it exists and connects
JUMP='transform:[ "$FZF_MATCH_COUNT" -ge "$FZF_KEY" ] && echo "pos($FZF_KEY)+accept"'
# "#" toggles digit mode: jump (default, digits pick a row) <-> type (digits go to the filter, 󰌌 shown before the colon)
TOGGLE='transform:case "$FZF_PROMPT" in *󰌌*) echo "rebind(1,2,3,4,5,6,7,8,9)+change-prompt(${FZF_PROMPT/ 󰌌 : /: })" ;; *) echo "unbind(1,2,3,4,5,6,7,8,9)+change-prompt(${FZF_PROMPT/: / 󰌌 : })" ;; esac'
session="$(
    sesh list -t -i | eval "$NUM" | fzf-tmux -x 100 -y 100 -p 100%,100% --height 100% \
        --no-sort --ansi --border-label " Tmux Session Manager " --prompt "${itmux} Tmux: " \
        --header "[${b}${k}󰘴a${n}]:All ${y}⚡${n} ${sep} [${b}${k}󰘴t${n}]:Tmux ${bl}${n}  ${sep} [${b}${k}󰘴x${n}]:Z ${izoxide} ${sep} [${b}${k}󰘴g${n}]:Conf ${iconfigured}  ${sep} [${b}${k}󰘴f${n}]:Find ${isearch}  ${sep} [${b}${k}󰘴n${n}]:New ${inew}  ${sep} [${b}${k}󰘴d${n}]:Kill ${ikill}  ${sep} [${b}${k}󰘴r${n}]:Rename ${irename} ${sep} [${b}${k}#${n}]:Digits" \
        --bind 'tab:down,btab:up' \
        --bind "1:$JUMP" --bind "2:$JUMP" --bind "3:$JUMP" --bind "4:$JUMP" --bind "5:$JUMP" \
        --bind "6:$JUMP" --bind "7:$JUMP" --bind "8:$JUMP" --bind "9:$JUMP" \
        --bind "#:$TOGGLE" \
        --bind "ctrl-a:change-prompt(${y}⚡${n} Sesh All: )+rebind($DIGITS)+reload(sesh list -i | $NUM)" \
        --bind "ctrl-t:change-prompt(${itmux} Tmux: )+rebind($DIGITS)+reload(sesh list -t -i | $NUM)" \
        --bind "ctrl-x:change-prompt(${izoxide} Zoxide: )+rebind($DIGITS)+reload(sesh list -z -i | $NUM)" \
        --bind "ctrl-g:change-prompt(${iconfigured}  Conf: )+rebind($DIGITS)+reload(sesh list -c -i | $NUM)" \
        --bind 'ctrl-f:change-prompt(  Find: )+reload((fd -H -d 5 -t d -E ".*" -E "*books*" . ~ | xargs -I folder printf " folder\n" ; \
                                                    fd -H -d 3 -t d -p ~/.tmux ~ | xargs -I folder printf " folder\n" ; \
                                                    fd -H -d 2 -t d -p ~/.config ~ | xargs -I folder printf " folder\n" ; \
                                                    fd -H -d 5 -t d -p ~/.local/share/nvim/lazy ~ | xargs -I folder printf " folder\n") | '"$NUM"')' \
        --bind 'ctrl-d:execute(tmux has-session -t $(echo {} | cut -c5-) 2>/dev/null && tmux kill-session -t $(echo {} | cut -c5-))+change-prompt( Tmux Kill: )+reload(sesh list -t -i | '"$NUM"')' \
        --bind 'ctrl-n:execute([ -n "{q}" ] && (tmux has-session -t {q} 2>/dev/null || tmux new-session -s {q} -n {q} -d))+change-prompt(  Tmux New: )+reload(sesh list -t -i | '"$NUM"')' \
        --bind 'ctrl-r:execute(old=$(echo {} | cut -c5-); tmux has-session -t "$old" 2>/dev/null && new=$(bash -c "read -e -i \"\$1\" -p \"Rename to: \" n && printf %s \"\$n\"" _ "$old") && [ -n "$new" ] && tmux rename-session -t "$old" "$new")+change-prompt(󰑕  Tmux Rename: )+reload(sesh list -t -i | '"$NUM"')' \
        --preview 'n={}; n=${n#????}; tmux capture-pane -peJt "$n" 2>/dev/null || eza --tree --icons --level=1 --color=always --group-directories-first "$(eval echo "$n")"' \
        --preview-window=down,65%
)"

zle reset-prompt > /dev/null 2>&1 || true
[[ -n "$session" ]] && sesh connect "${session#??}"
#echo "$session"

# else
#
#     session="$(
#         sesh list -t -i | fzf-tmux -x 100 -y 100 -p 100%,90% --height 90% \
    #             --no-sort --ansi --border-label " Tmux Session Manager " --prompt "${bl}${n} Tmux: " \
    #             --header "[${b}${k}󰘴a${n}]:All ${y}⚡${n} ${sep} [${b}${k}󰘴t${n}]:Tmux ${itmux} ${sep} [${b}${k}󰘴x${n}]:Z ${izoxide} ${sep} [${b}${k}󰘴g${n}]:Conf ${iconfigured}  ${sep} [${b}${k}󰘴f${n}]:Find ${isearch}  ${sep} [${b}${k}󰘴n${n}]:New ${inew}  ${sep} [${b}${k}󰘴d${n}]:Kill ${ikill}" \
    #             --bind 'tab:down,btab:up' \
    #             --bind "ctrl-a:change-prompt(${y}⚡${n} Sesh All: )+reload(sesh list -i)" \
    #             --bind "ctrl-t:change-prompt(${itmux} Tmux: )+reload(sesh list -t -i)" \
    #             --bind "ctrl-x:change-prompt(${izoxide} Zoxide: )+reload(sesh list -z -i)" \
    #             --bind "ctrl-g:change-prompt(${iconfigured}  Conf: )+reload(sesh list -c -i)" \
    #             --bind 'ctrl-f:change-prompt(  Find: )+reload((fd -H -d 5 -t d -E ".*" -E "*books*" . ~ | xargs -I folder printf "\e[34m\e[0m folder\n" ; \
    #                                                       fd -H -d 3 -t d -p ~/.tmux ~ | xargs -I folder printf "\e[34m\e[0m folder\n" ; \
    #                                                       fd -H -d 2 -t d -p ~/.config ~ | xargs -I folder printf "\e[34m\e[0m folder\n" ; \
    #                                                       fd -H -d 5 -t d -p ~/.local/share/nvim/lazy ~ | xargs -I folder printf "\e[34m\e[0m folder\n"))' \
    #             --bind 'ctrl-d:execute(tmux has-session -t $(echo {} | cut -c4-) 2>/dev/null && tmux kill-session -t $(echo {} | cut -c4-))+change-prompt( Tmux Kill: )+reload(sesh list -t -i)' \
    #             --bind 'ctrl-n:execute([ -n "{q}" ] && (tmux has-session -t {q} 2>/dev/null || tmux new-session -s {q} -n {q} -d))+change-prompt( Tmux New: )+reload(sesh list -t -i)' \
    #             --preview 'tmux has-session -t $(echo {} | cut -c3-) 2>/dev/null && tmux capture-pane -peJt $(echo {} | cut -c3-) || eza --tree --icons --level=1 --color=always --group-directories-first "$(eval echo $(echo {} | cut -c3-))"' \
    #             --preview-window=down,75%
#     )"
#
#     zle reset-prompt > /dev/null 2>&1 || true
#     [[ -n "$session" ]] && sesh connect "$session"
#
# fi