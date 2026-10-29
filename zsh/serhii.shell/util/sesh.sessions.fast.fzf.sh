#!/usr/bin/env bash
# Tmux Session Manager (fast variant): minimal process spawns, same look as sesh.sessions.full.fzf.sh.
# - no fzf-tmux layer: inside tmux it re-runs itself directly in a tmux popup
# - no tput calls: escape sequences inlined
# - rows 1-9 are numbered and a digit picks that row and connects (default);
#   ctrl-d toggles to typing digits into the filter (shown as 󰌌 before the colon of the title, with a space so the terminal can draw it over two cells) and back,
#   any source key (ctrl-a/t/z/g) or action (ctrl-n/x) returns to jump mode
# - no preview: nothing is spawned while moving the cursor
# - no find (ctrl-f) / rename (ctrl-r): use sesh.sessions.full.fzf.sh (prefix+S) for those
# Used by tmux (prefix+t, via run-shell so the script sizes the popup) and zsh (alt-s / ctrl-f widget in tmux.scripts.sh).

# popup width: 70% of the client, but never narrower than the one-line hints need
MIN_W=103 # hints 95 cells (⚡ is double-width) + fzf border 2 + gutter 2 + 2: fits from 101 (measured) + margin 2
if [ -n "$TMUX" ] && [ -z "$SESH_FAST_POPUP" ]; then
    case "$0" in /*) self="$0" ;; *) self="$PWD/$0" ;; esac
    cw=$(tmux display-message -p '#{client_width}'); cw=${cw:-200}; w=$((cw * 70 / 100))
    [ "$w" -lt "$MIN_W" ] && w=$MIN_W; [ "$w" -gt "$cw" ] && w=$cw
    exec tmux display-popup -E -B -w "$w" -h 60% "SESH_FAST_POPUP=1 '$self'"
fi

b=$'\033[1m'
n=$'\033[0m'

s=$'\033[35m'
k=$'\033[32m'  # green
bl=$'\033[34m' # blue
y=$'\033[33m'  # yellow

sep="${s}${n}"

itmux=$'\033[38;5;4m\033[0m'
izoxide=$'\033[38;5;14m󱓞\033[0m'
iconfigured=$'\033[38;5;110m\033[0m'

inew=$'\033[38;5;32m\033[0m'
ikill=$'\033[38;5;1m\033[0m'

# marker shown in the title while digits are typed into the filter
mtype='󰌌'

# Row format: "<1-9> <icon> <name>" for the first nine rows, "  <icon> <name>" after that.
# Strip 4 chars (${n#????}) to get the bare name; sesh connect strips the icon itself, so it gets ${session#??}.
NUM="awk 'NR<=9{print NR\" \"\$0;next}{print \"  \"\$0}'"
DIGITS='1,2,3,4,5,6,7,8,9'
# a digit picks that row if it exists and connects
JUMP='transform:[ "$FZF_MATCH_COUNT" -ge "$FZF_KEY" ] && echo "pos($FZF_KEY)+accept"'
# ctrl-d toggles the digit mode: inserts/removes the marker before the colon of the title
TOGGLE="transform:case \"\$FZF_PROMPT\" in *$mtype*) echo \"rebind($DIGITS)+change-prompt(\${FZF_PROMPT/ $mtype : /: })\" ;; *) echo \"unbind($DIGITS)+change-prompt(\${FZF_PROMPT/: / $mtype : })\" ;; esac"

session="$(
    sesh list -t -i | eval "$NUM" | fzf --height 100% --border --border-label " Tmux Session Manager " --no-sort --ansi \
        --prompt "${itmux} Tmux: " \
        --header "[${b}${k}󰘴a${n}]:All ${y}⚡${n} ${sep} [${b}${k}󰘴t${n}]:Tmux ${bl}${n}  ${sep} [${b}${k}󰘴z${n}]:Z ${izoxide} ${sep} [${b}${k}󰘴g${n}]:Conf ${iconfigured}  ${sep} [${b}${k}󰘴n${n}]:New ${inew}  ${sep} [${b}${k}󰘴x${n}]:Kill ${ikill}  ${sep} [${b}${k}󰘴d${n}]:Digits" \
        --bind 'tab:down,btab:up' \
        --bind "1:$JUMP" --bind "2:$JUMP" --bind "3:$JUMP" --bind "4:$JUMP" --bind "5:$JUMP" \
        --bind "6:$JUMP" --bind "7:$JUMP" --bind "8:$JUMP" --bind "9:$JUMP" \
        --bind "ctrl-d:$TOGGLE" \
        --bind "ctrl-a:change-prompt(${y}⚡${n} Sesh All: )+rebind($DIGITS)+reload(sesh list -i | $NUM)" \
        --bind "ctrl-t:change-prompt(${itmux} Tmux: )+rebind($DIGITS)+reload(sesh list -t -i | $NUM)" \
        --bind "ctrl-z:change-prompt(${izoxide} Zoxide: )+rebind($DIGITS)+reload(sesh list -z -i | $NUM)" \
        --bind "ctrl-g:change-prompt(${iconfigured}  Conf: )+rebind($DIGITS)+reload(sesh list -c -i | $NUM)" \
        --bind 'ctrl-x:execute(n={}; tmux kill-session -t "${n#????}" 2>/dev/null)+change-prompt( Tmux Kill: )+rebind('"$DIGITS"')+reload(sesh list -t -i | '"$NUM"')' \
        --bind 'ctrl-n:execute([ -n {q} ] && tmux new-session -d -s {q} -n {q} 2>/dev/null)+change-prompt(  Tmux New: )+rebind('"$DIGITS"')+reload(sesh list -t -i | '"$NUM"')'
)"

[ -n "$session" ] && exec sesh connect "${session#??}"
exit 0 # nothing picked (esc / ctrl-c): leave quietly, otherwise tmux reports "returned 1"
