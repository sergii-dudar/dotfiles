#!/usr/bin/env bash

# little customized this great script https://gist.github.com/pbnj/67c16c37918ba40bbb233b97f3e38456
# see there to get explanation of most part

set -uo pipefail

SESSION_POPUP_NAME="${1:-}"
# add `_popup` only if opened from parent session, if from `popup` session, ignore
[[ $SESSION_POPUP_NAME != *_popup ]] && SESSION_POPUP_NAME+="_popup"

# mode:
#   (empty) - toggle bottom panel (show/hide)
#   float   - toggle popup (float) terminal
#   max     - toggle bottom panel maximized (zoomed), second press returns the state it was before (shown/hidden)
MODE="${2:-}"
LIST_PANES="$(tmux list-panes -F '#F')"
PANE_ZOOMED="$(echo "${LIST_PANES}" | grep Z)"
PANE_COUNT="$(echo "${LIST_PANES}" | wc -l | bc)"

# first pane is the main one, all others are the bottom panel
PANES="$(tmux list-panes -F '#{pane_id} #{pane_active}')"
MAIN_PANE="$(echo "${PANES}" | awk 'NR == 1 { print $1 }')"
BOTTOM_PANE="$(echo "${PANES}" | awk 'END { print $1 }')"
ACTIVE_PANE="$(echo "${PANES}" | awk '$2 == 1 { print $1 }')"
# in case of several bottom panes, prefer the one that is in use
[ "${ACTIVE_PANE}" != "${MAIN_PANE}" ] && BOTTOM_PANE="${ACTIVE_PANE}"

# window option: what the bottom panel was (shown|hidden) before it was maximized
MAX_RESTORE_OPTION="@bottom_panel_max_restore"

if [ "${MODE}" = "float" ]; then
    if [ "$(tmux display-message -p -F "#{session_name}")" = "$SESSION_POPUP_NAME" ]; then
        tmux detach-client
    else
        # --- https://man.openbsd.org/OpenBSD-current/man1/tmux.1#display-popup ---
        # tmux popup -d '#{pane_current_path}' -xC -yC -w90% -h80% -E "tmux attach -t $SESSION_POPUP_NAME || tmux new -s $SESSION_POPUP_NAME"
        tmux popup -d '#{pane_current_path}' -xC -yC -w90% -h80% -E -T "$SESSION_POPUP_NAME" \
            "tmux attach -t $SESSION_POPUP_NAME || tmux new -s $SESSION_POPUP_NAME \; set -t $SESSION_POPUP_NAME status off"
    fi
elif [ "${MODE}" = "max" ]; then
    if [ "${PANE_COUNT}" = 1 ]; then
        # no bottom panel yet: create it already maximized
        tmux split-window -c "#{pane_current_path}" \; resize-pane -Z \; set-option -w "${MAX_RESTORE_OPTION}" hidden
    elif [ -z "${PANE_ZOOMED}" ]; then
        # bottom panel is shown: maximize
        tmux resize-pane -Z -t "${BOTTOM_PANE}" \; set-option -w "${MAX_RESTORE_OPTION}" shown
    elif [ "${ACTIVE_PANE}" = "${MAIN_PANE}" ]; then
        # bottom panel is hidden: maximize (-Z moves zoom to the bottom panel)
        tmux select-pane -Z -t "${BOTTOM_PANE}" \; set-option -w "${MAX_RESTORE_OPTION}" hidden
    else
        # bottom panel is maximized: return the state it was before
        if [ "$(tmux show-options -wqv "${MAX_RESTORE_OPTION}")" = "hidden" ]; then
            tmux select-pane -Z -t "${MAIN_PANE}" \; set-option -wu "${MAX_RESTORE_OPTION}"
        else
            tmux resize-pane -Z \; set-option -wu "${MAX_RESTORE_OPTION}"
        fi
    fi
else
    if [ "${PANE_COUNT}" = 1 ]; then
        tmux split-window -c "#{pane_current_path}"
    elif [ -n "${PANE_ZOOMED}" ]; then
        if [ "${ACTIVE_PANE}" = "${MAIN_PANE}" ]; then
            tmux select-pane -t:.-
        else
            # bottom panel is maximized: hide it
            tmux select-pane -Z -t "${MAIN_PANE}" \; set-option -wu "${MAX_RESTORE_OPTION}"
        fi
    else
        tmux resize-pane -Z -t1
    fi
fi