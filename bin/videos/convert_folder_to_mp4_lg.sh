#!/usr/bin/env bash

eza --oneline --color=never --icons=never | xargs -n 1 ~/dotfiles/bin/videos/convert_to_mp4_lg.sh