#!/usr/bin/env bash

right_items_common=(
    icon.padding_left=6
    icon.padding_right=5
    label.padding_left=0
    label.padding_right=6
    background.padding_right=2
    #background.padding_left=0
)

left_items_common=(
    icon.padding_left=6
    icon.padding_right=5
    label.padding_left=0
    label.padding_right=6
    #background.padding_right=2
    background.padding_left=2
)

center_items_common=(
    icon.padding_left=10
    icon.padding_right=5
    label.padding_left=0
    label.padding_right=10
)

popup_events=(
    mouse.entered
    mouse.exited
    mouse.exited.global
)

popup() {
    sketchybar --set "$NAME" popup.drawing="$1"
}