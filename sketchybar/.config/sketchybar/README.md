## Info

In case `display notification` is not working from shell command, do next one time:

- run `Script Editor`
- put next command `display notification "Hello from me" with title "Hello"` and execute (push play button)
- in ask permissions to show notification - allow

After this, in from shell notification commands should works correctly, check in terminal: `osascript -e 'display notification "Hello from me" with title "Hello"'`

# All extensions to open: <https://gist.github.com/rmcdongit/f66ff91e0dad78d4d6346a75ded4b751?permalink_comment_id=5507723#gistcomment-5507723>
