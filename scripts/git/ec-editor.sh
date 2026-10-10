#!/usr/bin/env bash
# $EDITOR for ec (easy-conflict, https://github.com/chojs23/ec).
# `e` in ec runs `$EDITOR <merged file>` as one executable with no shell parsing, so EDITOR="nvim +DiffviewOpen"
# cannot work; this wrapper opens the file in nvim with diffview's merge view (nvim: plugins/editor/git.lua)
# already open. Resolve, `q` closes the view, `ZZ` writes and returns to ec, which reloads the file.
# Wired in git/.gitconfig ([mergetool "ec"]) and zsh/serhii.shell/aliases.sh (`ec` function).
set -euo pipefail
exec nvim -c "DiffviewOpen" -- "$@"
