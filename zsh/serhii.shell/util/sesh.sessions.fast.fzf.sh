#!/usr/bin/env bash
# Tmux Session Manager (fast variant): minimal process spawns, same look as sesh.sessions.full.fzf.sh.
# - no fzf-tmux layer: inside tmux it re-runs itself directly in a tmux popup
# - no tput calls: escape sequences inlined
# - rows 1-9 are numbered and a digit picks that row and connects (default);
#   "#" toggles to typing digits into the filter (shown as 󰌌 before the colon of the title) and back,
#   any source key (ctrl-a/t/x/g) or action (ctrl-n/d) returns to jump mode
# - preview hidden until toggled with ctrl-p, so cursor moves spawn nothing
# - no find (ctrl-f) / rename (ctrl-r): use sesh.sessions.full.fzf.sh (prefix+S) for those
# Used by tmux (prefix+t) and zsh (alt-s / ctrl-f widget in tmux.scripts.sh).

if [ -n "$TMUX" ] && [ -z "$SESH_FAST_POPUP" ]; then
case "$0" in /*) self="$0" ;; *) self="$PWD/$0" ;; esac
    exec tmux display-popup -E -B -w 70% -h 60% "SESH_FAST_POPUP=1 '$self'"
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

# marker shown in the title while digits are typed into the filter
mtype='󰌌'

# Row format: "<1-9> <icon> <name>" for the first nine rows, "  <icon> <name>" after that.
# Strip 4 chars (${n#????}) to get the bare name; sesh connect strips the icon itself, so it gets ${session#??}.
NUM="awk 'NR<=9{print NR\" \"\$0;next}{print \"  \"\$0}'"
DIGITS='1,2,3,4,5,6,7,8,9'
# a digit picks that row if it exists and connects
JUMP='transform:[ "$FZF_MATCH_COUNT" -ge "$FZF_KEY" ] && echo "pos($FZF_KEY)+accept"'
# "#" toggles the digit mode: inserts/removes the marker before the colon of the title
TOGGLE="transform:case \"\$FZF_PROMPT\" in *$mtype*) echo \"rebind($DIGITS)+change-prompt(\${FZF_PROMPT/ $mtype: /: })\" ;; *) echo \"unbind($DIGITS)+change-prompt(\${FZF_PROMPT/: / $mtype: })\" ;; esac"

session="$(
    sesh list -t -i | eval "$NUM" | fzf --height 100% --border=none --no-sort --ansi \
        --prompt "${itmux} Tmux: " \
        --header "[${b}${k}󰘴a${n}]:All ${y}⚡${n} ${sep} [${b}${k}󰘴t${n}]:Tmux ${bl}${n}  ${sep} [${b}${k}󰘴x${n}]:Z ${izoxide} ${sep} [${b}${k}󰘴g${n}]:Conf ${iconfigured}"$'\n'"[${b}${k}󰘴n${n}]:New ${inew}  ${sep} [${b}${k}󰘴d${n}]:Kill ${ikill}  ${sep} [${b}${k}󰘴p${n}]:Preview ${ipreview}  ${sep} [${b}${k}#${n}]:Digits" \
        --bind 'tab:down,btab:up' \
        --bind "1:$JUMP" --bind "2:$JUMP" --bind "3:$JUMP" --bind "4:$JUMP" --bind "5:$JUMP" \
        --bind "6:$JUMP" --bind "7:$JUMP" --bind "8:$JUMP" --bind "9:$JUMP" \
        --bind "#:$TOGGLE" \
        --bind "ctrl-a:change-prompt(${y}⚡${n} Sesh All: )+rebind($DIGITS)+reload(sesh list -i | $NUM)" \
        --bind "ctrl-t:change-prompt(${itmux} Tmux: )+rebind($DIGITS)+reload(sesh list -t -i | $NUM)" \
        --bind "ctrl-x:change-prompt(${izoxide} Zoxide: )+rebind($DIGITS)+reload(sesh list -z -i | $NUM)" \
        --bind "ctrl-g:change-prompt(${iconfigured}  Conf: )+rebind($DIGITS)+reload(sesh list -c -i | $NUM)" \
        --bind 'ctrl-d:execute(n={}; tmux kill-session -t "${n#????}" 2>/dev/null)+change-prompt( Tmux Kill: )+rebind('"$DIGITS"')+reload(sesh list -t -i | '"$NUM"')' \
        --bind 'ctrl-n:execute([ -n {q} ] && tmux new-session -d -s {q} -n {q} 2>/dev/null)+change-prompt(  Tmux New: )+rebind('"$DIGITS"')+reload(sesh list -t -i | '"$NUM"')' \
        --bind 'ctrl-p:toggle-preview' \
        --preview 'n={}; n=${n#????}; tmux capture-pane -peJt "$n" 2>/dev/null || eza --tree --icons --level=1 --color=always --group-directories-first "$(eval echo "$n")"' \
        --preview-window=down,65%,hidden
)"

[ -n "$session" ] && exec sesh connect "${session#??}"