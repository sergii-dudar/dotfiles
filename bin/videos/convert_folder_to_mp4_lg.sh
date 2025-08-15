#!/usr/bin/env bash

IN_DIR="$PWD"/$1
OUT_DIR="${IN_DIR}-converted"

cd "$IN_DIR/.." || exit 1

rm -r "$OUT_DIR" || echo "no $OUT_DIR to remove, ignore"
mkdir -p "$OUT_DIR"

cd "$IN_DIR" || exit 1

# eza --oneline --color=never --icons=never | xargs -n 1 ~/dotfiles/bin/videos/convert_to_mp4_lg.sh
# echo "---------$PWD"

eza --oneline --color=never --icons=never \
    | xargs -I {} ~/dotfiles/bin/videos/convert_to_mp4_lg.sh "{}" "$OUT_DIR"