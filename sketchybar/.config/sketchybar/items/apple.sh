#!/bin/sh

POPUP_OFF="sketchybar --set apple.logo popup.drawing=off"
POPUP_CLICK_SCRIPT="sketchybar --set \$NAME popup.drawing=toggle"

sketchybar --add item           apple.logo left                             \
    --set apple.logo     icon=$APPLE                                 \
    icon.color=$RED                             \
    padding_right=5                            \
    label.drawing=off                           \
    click_script="$POPUP_CLICK_SCRIPT"          \
    \
    --add item           apple.prefs popup.apple.logo                \
    --set apple.prefs    icon=$PREFERENCES                           \
    label="Preferences"                         \
    icon.color=$WHITE                           \
    label.color=$WHITE                          \
    click_script="open -a 'System Preferences';
$POPUP_OFF"                   \
                                                                            \
           --add item           apple.activity popup.apple.logo             \
           --set apple.activity icon=$ACTIVITY                              \
                                label="Activity"                            \
                                icon.color=$WHITE                           \
                                label.color=$WHITE                          \
                                click_script="open -a 'Activity Monitor';
$POPUP_OFF"\
                                                                            \
           --add item           apple.lock popup.apple.logo                 \
           --set apple.lock     icon=$LOCK                                  \
                                label="Lock Screen"                         \
                                icon.color=$WHITE                           \
                                label.color=$WHITE                          \
                                click_script="pmset displaysleepnow;
$POPUP_OFF"