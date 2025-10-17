#!/usr/bin/env bash

if [[ $# -eq 1 ]]; then
    selected_dir=$1
else
    selected_dir=$(find ~/work/builds ~/projects ~/ ~/work ~/personal ~/personal/yt -mindepth 1 -maxdepth 1 -type d | fzf)
fi

if [[ -z $selected_dir ]]; then
    exit 0
fi

#selected_name=$(basename "$selected" | tr . _)
selected_name="$selected_dir"
tmux_running=$(pgrep tmux)

if [[ -z $TMUX ]] && [[ -z $tmux_running ]]; then
    tmux new-session -s "$selected_name" -c "$selected_dir"
    exit 0
fi

if ! tmux has-session -t="$selected_name" 2> /dev/null; then
    tmux new-session -ds "$selected_name" -c "$selected_dir"
fi

tmux switch-client -t "$selected_name"