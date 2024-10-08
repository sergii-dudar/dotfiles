# shares sesh script that using by zsh (alt-s) and tmux (prefix+t)
bold=$(tput bold)
normal=$(tput sgr0)
session="$(
  sesh list -t -i | fzf-tmux -p 55%,60% \
        --no-sort --ansi --border-label " Tmux Session Manager " --prompt '🪟 Tmux: ' --pointer '👉' \
        --header "   ${bold}󰘴a${normal}: Sesh All ⚡  ${bold}󰘴t${normal}: Tmux 🪟  ${bold}󰘴x${normal}: Zoxide 📁  ${bold}󰘴f${normal}: Find 🔎  ${bold}󰘴d${normal}: Tmux Kill ❌ " \
        --bind 'tab:down,btab:up' \
        --bind 'ctrl-a:change-prompt(⚡ Sesh All: )+reload(sesh list -i)' \
        --bind 'ctrl-t:change-prompt(🪟 Tmux: )+reload(sesh list -t -i)' \
        --bind 'ctrl-x:change-prompt(📁 Zoxide: )+reload(sesh list -z -i)' \
        --bind 'ctrl-f:change-prompt(🔎 Find: )+reload(fd -H -d 5 -t d -E ".*" . ~ ; \
                                                  fd -H -d 3 -t d -p ~/.tmux ~ ; \
                                                  fd -H -d 2 -t d -p ~/.config ~ ; \
                                                  fd -H -d 5 -t d -p ~/.local/share/nvim/lazy ~)' \
        --bind 'ctrl-d:execute(tmux kill-session -t {})+change-prompt(❌ Tmux Kill: )+reload(sesh list)'
)"

[[ -n "$session" ]] && sesh connect "$session"

#--header '  ^a all ^t tmux ^g configs ^x zoxide ^d tmux kill ^f find' \
#--bind 'ctrl-g:change-prompt(⚙️  )+reload(sesh list -c)' \