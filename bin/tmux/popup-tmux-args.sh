#!/usr/bin/env bash

# little customized this great script https://gist.github.com/pbnj/67c16c37918ba40bbb233b97f3e38456
# see there to get explanation of most part

set -uo pipefail

SESSION_POPUP_NAME="${1:-}"
# add `_popup` only if opened from parent session, if from `popup` session, ignore
[[ $SESSION_POPUP_NAME != *_popup ]] && SESSION_POPUP_NAME+="_popup"

FLOAT_TERM="${2:-}"
LIST_PANES="$(tmux list-panes -F '#F' )"
PANE_ZOOMED="$(echo "${LIST_PANES}" | grep Z)"
PANE_COUNT="$(echo "${LIST_PANES}" | wc -l | bc)"

if [ -n "${FLOAT_TERM}" ]; then
  if [ "$(tmux display-message -p -F "#{session_name}")" = "$SESSION_POPUP_NAME" ]; then
    tmux detach-client
  else
    # tmux popup -d '#{pane_current_path}' -xC -yC -w90% -h80% -E "tmux attach -t $SESSION_POPUP_NAME || tmux new -s $SESSION_POPUP_NAME"
    tmux popup -d '#{pane_current_path}' -xC -yC -w90% -h80% -E \
        "tmux attach -t $SESSION_POPUP_NAME || tmux new -s $SESSION_POPUP_NAME \; set -t $SESSION_POPUP_NAME status off"
  fi
else
  if [ "${PANE_COUNT}" = 1 ]; then
    tmux split-window -c "#{pane_current_path}"
  elif [ -n "${PANE_ZOOMED}" ]; then
    tmux select-pane -t:.-
  else
    tmux resize-pane -Z -t1
  fi
fi