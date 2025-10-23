#!/usr/bin/env bash

COMMAND=$1
# COMMAND=/opt/homebrew/bin/btop
# osascript -e 'tell application "iTerm"
#     activate
#     set newWindow to (create window with default profile)
#     tell current session of newWindow
#         write text "'"$COMMAND"'"
#     end tell
# end tell'
osascript -e '
if application "iTerm" is running then
    tell application "iTerm"
        set newWindow to (create window with default profile)
        tell current session of newWindow
            write text "'"$COMMAND"'"
        end tell
        activate
    end tell
else
    tell application "iTerm"
        activate
        -- Wait for window 1 to exist (timeout after ~5 seconds)
        repeat 50 times
            if (exists window 1) then exit repeat
            delay 0.1
        end repeat
        -- Wait for session 1 to exist
        repeat 50 times
            if (exists current session of window 1) then exit repeat
            delay 0.1
        end repeat
        tell current session of window 1
            write text "'"$COMMAND"'"
        end tell
    end tell
end if
'


~/.config/aerospace/size_and_center_float.sh "iTerm"