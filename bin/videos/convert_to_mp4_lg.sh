#!/usr/bin/env bash

INPUT=$1

ffmpeg -i "$INPUT" \
    -c:v copy \
    -c:a aac -b:a 192k \
    -movflags +faststart \
    "con-${INPUT}.mp4"

# ffmpeg -i S01E001_Епізод\ 1.mkv -c copy -movflags +faststart S01E001_Епізод\ 1-02.mp4
#
# ffmpeg -i S01E001_Епізод\ 1.mkv \
    #   -c:v libx264 -preset slow -crf 18 -pix_fmt yuv420p \
    #   -c:a aac -b:a 192k \
    #   -movflags +faststart \
    #   S01E001_Епізод\ 1-03.mkv.mp4