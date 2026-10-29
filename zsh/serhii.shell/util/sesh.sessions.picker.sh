#!/usr/bin/env bash
# Tmux Session Manager (sesh picker variant): sesh's built-in TUI, a single Go process, no fzf.
# - filters in-process; "#" then 1-9 jumps to a numbered row (numbers follow the filtered list)
# - "/" alias mode, ctrl-x kills the session (with confirmation), enter connects
# - sources are fixed at launch: tmux sessions by default; extra sesh picker flags pass through
#   (e.g. -z zoxide, -c config, -T tmuxinator, --preview; see `sesh picker --help` and [tui] in sesh.toml)
# - no source switching, new-from-query, rename or find: use the fzf variants for those
#   (sesh.sessions.fast.fzf.sh on prefix+t, sesh.sessions.full.fzf.sh on prefix+T)
# Inside tmux it re-runs itself in a small popup; outside tmux it runs inline.

if [ -n "$TMUX" ] && [ -z "$SESH_PICKER_POPUP" ]; then
    case "$0" in /*) self="$0" ;; *) self="$PWD/$0" ;; esac
    exec tmux display-popup -E -B -w 30% -h 60% "SESH_PICKER_POPUP=1 '$self' $*"
fi

exec sesh picker -t -i "$@"
