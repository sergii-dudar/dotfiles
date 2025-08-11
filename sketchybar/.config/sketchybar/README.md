## Info

In case `display notification` is not working from shell command, do next one time:

- run `Script Editor`
- put next command `display notification "Hello from me" with title "Hello"` and execute (push play button)
- in ask permissions to show notification - allow

After this, in from shell notification commands should works correctly, check in terminal: `osascript -e 'display notification "Hello from me" with title "Hello"'`