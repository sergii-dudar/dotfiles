#!/usr/bin/env bash

IN_VIDEO="$PWD"/$1
OUT_VIDEO="${IN_VIDEO}-converted"

ffmpeg -i "$IN_VIDEO" \
    -c:v copy \
    -c:a aac -b:a 192k \
    -movflags +faststart \
    "${OUT_VIDEO}.mp4"