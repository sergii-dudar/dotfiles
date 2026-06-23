PREV_NOT_POPUP_SESSION=$(\
tmux list-sessions -F '#{session_last_attached} #{session_name}' | \
sort -rn | \
awk '{print $2}' | \
grep -v "_popup" | \
head -n 2 | \
tail -n 1)

[[ -n "$PREV_NOT_POPUP_SESSION" ]] && sesh connect "$PREV_NOT_POPUP_SESSION"