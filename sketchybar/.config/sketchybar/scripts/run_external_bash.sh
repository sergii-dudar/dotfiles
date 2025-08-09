#!/usr/bin/env bash

COMMAND=$1
# COMMAND=/opt/homebrew/bin/btop
osascript -e 'tell application "iTerm"
    activate
    set newWindow to (create window with default profile)
    tell current session of newWindow
        write text "'"$COMMAND"'"
    end tell
end tell'
sleep 0.2
~/.config/aerospace/size_and_center_float.sh "iTerm"
