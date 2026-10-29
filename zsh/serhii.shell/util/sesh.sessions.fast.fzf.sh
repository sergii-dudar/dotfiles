#!/usr/bin/env bash
# Tmux Session Manager (fast variant): minimal process spawns, same look as sesh.sessions.full.fzf.sh.
# - no fzf-tmux layer: tmux prefix+t opens the popup itself (run-shell -C in .tmux.conf sizes it with a format) and
#   sources this file into the popup shell with SESH_FAST_POPUP=1; the wrapper below is for the zsh widget (alt-s),
#   which runs the script and lets it open and size the popup
# - no tput calls: escape sequences inlined (the env shebang hop is paid only on the alt-s path; tmux sources the file)
# - row numbering is a shell loop, not awk: no extra process at start or on reload
# - rows 1-9 are numbered and a digit picks that row and connects (default);
#   ctrl-d toggles to typing digits into the filter (shown as 󰌌 before the colon of the title, with a space so the terminal can draw it over two cells) and back,
#   any source key (ctrl-a/t/z/g) or action (ctrl-n/x) returns to jump mode
# - no preview: nothing is spawned while moving the cursor
# - no find (ctrl-f) / rename (ctrl-r): use sesh.sessions.full.fzf.sh (prefix+S) for those
# Used by tmux (prefix+t, sourced) and zsh (alt-s / ctrl-f widget in tmux.scripts.sh, executed). Keep it bash- and zsh-compatible.

# popup width: 70% of the client, but never narrower than the one-line hints need (keep MIN_W in sync with .tmux.conf)
MIN_W=103 # hints 95 cells (⚡ is double-width) + fzf border 2 + gutter 2 + 2: fits from 101 (measured) + margin 2
if [ -n "$TMUX" ] && [ -z "$SESH_FAST_POPUP" ]; then
    case "$0" in /*) self="$0" ;; *) self="$PWD/$0" ;; esac
    # one tmux call: the width is a tmux format (70% of the client, at least MIN_W, at most 100%; the second threshold is
    # ceil(MIN_W / 0.7)), and the popup shell sources this file instead of executing it (one process less)
    exec tmux run-shell -C "display-popup -E -B -w #{?#{e|<:#{client_width},$MIN_W},100%,#{?#{e|<:#{client_width},$(( (MIN_W * 10 + 6) / 7 ))},$MIN_W,70%}} -h 60% 'SESH_FAST_POPUP=1; . $self'"
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
# (POSIX brace group, no parentheses: it is pasted after a pipe into fzf reload(...) actions, where a ")" would end the action.)
# Strip 4 chars (${n#????}) to get the bare name; sesh connect strips the icon itself, so it gets ${session#??}.
NUM='{ set -- 1 2 3 4 5 6 7 8 9; while IFS= read -r l; do if [ $# -gt 0 ]; then printf "%s %s\n" "$1" "$l"; shift; else printf "  %s\n" "$l"; fi; done; }'
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
