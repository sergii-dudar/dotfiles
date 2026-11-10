#!/bin/sh
# tmux-resurrect autosave on tmux's own timer. tmux-continuum does it by putting "#(continuum_save.sh)" into
# status-right, which makes tmux run that script (13 processes) every status-interval just to check whether the save
# interval has passed: with the slow process starts here that is ~10 000 spawns an hour for nothing.
# .tmux.conf runs this once after tpm, without arguments: it arms a chain in which tmux waits (run-shell -d), saves and
# arms the next link, so nothing runs between saves. Re-sourcing the config arms a new chain and the old one stops at
# its next link, because only the newest chain holds the token. Needs tmux >= 3.2 (run-shell -d); tmux runs it with /bin/sh.
save=~/.tmux/plugins/tmux-resurrect/scripts/save.sh
interval=$(tmux show -gqv @resurrect-autosave-interval) # seconds; default 15 min, like @continuum-save-interval

case "$1" in
    save)
        [ "$2" = "$(tmux show -gqv @resurrect-autosave-token)" ] || exit 0 # a chain armed before the last config reload
        "$save" quiet
        token=$2
        ;;
    *)
        token=$$
        tmux set -g @resurrect-autosave-token "$token"
        ;;
esac
tmux run-shell -b -d "${interval:-900}" "$0 save $token"