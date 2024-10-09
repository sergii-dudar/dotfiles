# shares sesh script that using by zsh (alt-s) and tmux (prefix+t)
bold=$(tput bold)
normal=$(tput sgr0)

session="$(
  sesh list -t -i | fzf-tmux -p 80%,60% \
        --no-sort --ansi --border-label " Tmux Session Manager " --prompt '🪟 Tmux: ' --pointer '👉' \
        --header \
$"▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰
█【${bold}󰘴a${normal}】All ⚡ █【${bold}󰘴t${normal}】Tmux 🪟 █【${bold}󰘴x${normal}】Z 📁 █【${bold}󰘴f${normal}】Find 🔎 █【${bold}󰘴n${normal}】New ❌ █【${bold}󰘴d${normal}】Kill ❌ █
▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰▰" \
        --bind 'tab:down,btab:up' \
        --bind 'ctrl-a:change-prompt(⚡ Sesh All: )+reload(sesh list -i)' \
        --bind 'ctrl-t:change-prompt(🪟 Tmux: )+reload(sesh list -t -i)' \
        --bind 'ctrl-x:change-prompt(📁 Zoxide: )+reload(sesh list -z -i)' \
        --bind 'ctrl-f:change-prompt(🔎 Find: )+reload(fd -H -d 5 -t d -E ".*" . ~ ; \
                                                  fd -H -d 3 -t d -p ~/.tmux ~ ; \
                                                  fd -H -d 2 -t d -p ~/.config ~ ; \
                                                  fd -H -d 5 -t d -p ~/.local/share/nvim/lazy ~)' \
        --bind 'ctrl-d:execute(tmux has-session -t {} 2>/dev/null && tmux kill-session -t {})+change-prompt(❌ Tmux Kill: )+reload(sesh list -t)' \
        --bind 'ctrl-n:execute([ -n "{q}" ] && (tmux has-session -t {q} 2>/dev/null || tmux new-session -s {q} -d))+change-prompt(❌ Tmux New: )+reload(sesh list -t -i)'
)"

[[ -n "$session" ]] && sesh connect "$session"

#--header '  ^a all ^t tmux ^g configs ^x zoxide ^d tmux kill ^f find' \
#--bind 'ctrl-g:change-prompt(⚙️  )+reload(sesh list -c)' \