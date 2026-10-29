#!/usr/bin/env bash
# Tmux Session Manager (fast variant): minimal process spawns.
# - no fzf-tmux layer: inside tmux it re-runs itself directly in a tmux popup
# - no tput calls: escape sequences inlined
# - preview hidden until toggled with ctrl-p, so cursor moves spawn nothing
# - rows 1-9 are numbered: "#" then a digit jumps to that row and connects;
#   digits filter as usual until "#" is pressed, any filter key (ctrl-a/t/x/g) disarms again
# - no find (ctrl-f) / rename (ctrl-r): use sesh.sessions.full.fzf.sh (prefix+T) for those
# Used by tmux (prefix+t) and zsh (alt-s / ctrl-f widget in tmux.scripts.sh).

if [ -n "$TMUX" ] && [ -z "$SESH_FAST_POPUP" ]; then
    case "$0" in /*) self="$0" ;; *) self="$PWD/$0" ;; esac
    exec tmux display-popup -E -B -w 45% -h 60% "SESH_FAST_POPUP=1 '$self'"
fi

b=$'\033[1m'
n=$'\033[0m'
k=$'\033[32m'  # green
y=$'\033[33m'  # yellow

itmux=$'\033[38;5;4m\033[0m'
izoxide=$'\033[38;5;14m󱓞\033[0m'
iconfigured=$'\033[38;5;110m\033[0m'

# Row format: "<1-9> <icon> <name>" for the first nine rows, "  <icon> <name>" after that.
# Strip 4 chars (${n#????}) to get the bare name; sesh connect strips the icon itself, so it gets ${session#??}.
NUM="awk 'NR<=9{print NR\" \"\$0;next}{print \"  \"\$0}'"
DIGITS='1,2,3,4,5,6,7,8,9'
# digit keys are unbound at start; "#" rebinds them, then a digit jumps to that row if it exists and connects
JUMP='transform:[ "$FZF_MATCH_COUNT" -ge "$FZF_KEY" ] && echo "pos($FZF_KEY)+accept"'

session="$(
    sesh list -t -i | eval "$NUM" | fzf --height 100% --border=none --no-sort --ansi \
        --prompt "${itmux} Tmux: " \
        --header "${b}${k}󰘴a${n} All  ${b}${k}󰘴t${n} Tmux  ${b}${k}󰘴x${n} Zoxide  ${b}${k}󰘴g${n} Conf  ${b}${k}󰘴n${n} New  ${b}${k}󰘴d${n} Kill  ${b}${k}󰘴p${n} Preview  ${b}${k}#${n} Jump" \
        --bind 'tab:down,btab:up' \
        --bind "start:unbind($DIGITS)" \
        --bind "1:$JUMP" --bind "2:$JUMP" --bind "3:$JUMP" --bind "4:$JUMP" --bind "5:$JUMP" \
        --bind "6:$JUMP" --bind "7:$JUMP" --bind "8:$JUMP" --bind "9:$JUMP" \
        --bind "#:rebind($DIGITS)+change-prompt(${itmux} Jump 1-9: )" \
        --bind "ctrl-a:change-prompt(${y}⚡${n} Sesh All: )+unbind($DIGITS)+reload(sesh list -i | $NUM)" \
        --bind "ctrl-t:change-prompt(${itmux} Tmux: )+unbind($DIGITS)+reload(sesh list -t -i | $NUM)" \
        --bind "ctrl-x:change-prompt(${izoxide} Zoxide: )+unbind($DIGITS)+reload(sesh list -z -i | $NUM)" \
        --bind "ctrl-g:change-prompt(${iconfigured}  Conf: )+unbind($DIGITS)+reload(sesh list -c -i | $NUM)" \
        --bind 'ctrl-d:execute(n={}; tmux kill-session -t "${n#????}" 2>/dev/null)+change-prompt( Tmux Kill: )+reload(sesh list -t -i | '"$NUM"')' \
        --bind 'ctrl-n:execute([ -n {q} ] && tmux new-session -d -s {q} -n {q} 2>/dev/null)+change-prompt(  Tmux New: )+reload(sesh list -t -i | '"$NUM"')' \
        --bind 'ctrl-p:toggle-preview' \
        --preview 'n={}; n=${n#????}; tmux capture-pane -peJt "$n" 2>/dev/null || eza --tree --icons --level=1 --color=always --group-directories-first "$(eval echo "$n")"' \
        --preview-window=down,65%,hidden
)"

[ -n "$session" ] && exec sesh connect "${session#??}"
