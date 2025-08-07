#!/usr/bin/env bash

sketchybar --add item cpu.percent right \
    --set cpu.percent \
    icon=" " \
    label="8%" \
    icon.font="CaskaydiaCove Nerd Font:Bold:17.0" \
    label.font="CaskaydiaCove Nerd Font:Bold:16.0" \
    update_freq=5 \
    mach_helper="$HELPER"

# sketchybar --add item        cpu.top right                 \
    #            --set cpu.top     label.font="$FONT:Semibold:7" \
    #                              label=CPU                     \
    #                              icon.drawing=off              \
    #                              width=0                       \
    #                              padding_right=40              \
    #                              y_offset=6                    \
    #                                                            \
    #            --add item        cpu.percent right             \
    #            --set cpu.percent label.font="$FONT:Heavy:12"   \
    #                              label=CPU                     \
    #                              y_offset=-4                   \
    #                              padding_right=40              \
    #                              width=55                      \
    #                              icon.drawing=off              \
    #                              update_freq=2                 \
    #                              mach_helper="$HELPER"         \
    #                                                            \
    #            --add graph       cpu.sys right 75              \
    #            --set cpu.sys     width=0                       \
    #                              graph.color=$RED              \
    #                              graph.fill_color=$RED         \
    #                              label.drawing=off             \
    #                              icon.drawing=off              \
    #                              padding_right=25              \
    #                              background.height=30          \
    #                              background.drawing=on         \
    #                              background.color=$TRANSPARENT \
    #                                                            \
    #            --add graph       cpu.user right 75             \
    #            --set cpu.user    graph.color=$BLUE             \
    #                              label.drawing=off             \
    #                              icon.drawing=off              \
    #                              padding_right=25              \
    #                              background.height=30          \
    #                              background.drawing=on         \
    #                              background.color=$TRANSPARENT