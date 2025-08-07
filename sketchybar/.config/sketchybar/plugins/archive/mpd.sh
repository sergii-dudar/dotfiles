#!/usr/bin/env bash

if [ $(mpc status | wc -l | tr -d ' ') != 3  ]; then
    output=""
    icon=""
else
    artist=$(mpc current -f %artist%)
    song=$(mpc current -f %title%)
    status=$(mpc current %state%)

    if [ $status = "playing" ]; then
        icon="􀑬"
    else
        icon="􀑬"
    fi

    output="${artist} • ${song}"
fi

sketchybar -m --set mpd icon="${icon}" \
    --set mpd label="${output}"