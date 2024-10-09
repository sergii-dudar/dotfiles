# shares sesh script that using by zsh (alt-s) and tmux (prefix+t)
b=$(tput bold)
n=$(tput sgr0)

session="$(
  sesh list -t -i | fzf-tmux -x 100 -y 100 -p 100%,90% --height 90% \
        --no-sort --ansi --border-label " Tmux Session Manager " --prompt '🪟 Tmux: ' --pointer '👉' \
        --header \
"▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰
█【${b}󰘴a${n}】All ⚡ █【${b}󰘴t${n}】Tmux 🪟 █【${b}󰘴x${n}】Z 📁 █【${b}󰘴f${n}】Find 🔎 █【${b}󰘴n${n}】New 🆕 █【${b}󰘴d${n}】Kill ❌ █
▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰" \
        --bind 'tab:down,btab:up' \
        --bind 'ctrl-a:change-prompt(⚡ Sesh All: )+reload(sesh list -i)' \
        --bind 'ctrl-t:change-prompt(🪟 Tmux: )+reload(sesh list -t -i)' \
        --bind 'ctrl-x:change-prompt(📁 Zoxide: )+reload(sesh list -z -i)' \
        --bind 'ctrl-f:change-prompt(🔎 Find: )+reload((fd -H -d 5 -t d -E ".*" -E "*books*" . ~ | xargs -I folder printf "\e[34m\e[0m folder\n" ; \
                                                  fd -H -d 3 -t d -p ~/.tmux ~ | xargs -I folder printf "\e[34m\e[0m folder\n" ; \
                                                  fd -H -d 2 -t d -p ~/.config ~ | xargs -I folder printf "\e[34m\e[0m folder\n" ; \
                                                  fd -H -d 5 -t d -p ~/.local/share/nvim/lazy ~ | xargs -I folder printf "\e[34m\e[0m folder\n"))' \
        --bind 'ctrl-d:execute(tmux has-session -t $(echo {} | cut -c4-) 2>/dev/null && tmux kill-session -t $(echo {} | cut -c4-))+change-prompt(❌ Tmux Kill: )+reload(sesh list -t -i)' \
        --bind 'ctrl-n:execute([ -n "{q}" ] && (tmux has-session -t {q} 2>/dev/null || tmux new-session -s {q} -n {q} -d))+change-prompt(🆕 Tmux New: )+reload(sesh list -t -i)' \
        --preview 'tmux has-session -t $(echo {} | cut -c4-) 2>/dev/null && tmux capture-pane -peJt $(echo {} | cut -c4-) || eza --tree --icons --level=1 --color=always --group-directories-first $(eval echo $(echo {} | cut -c4-))' \
        --preview-window=down,60%
)"

[[ -n "$session" ]] && sesh connect "$session"