#!/usr/bin/env bash

IN_FILE_NAME=$1
OUTPUT_DIR=$2

# echo "---------$IN_FILE_NAME"
# echo "---------$OUTPUT_DIR/${IN_FILE_NAME}.mp4"

# ffmpeg -i "$IN_FILE_NAME" \
    #     -c:v copy \
    #     -c:a aac -b:a 192k \
    #     -movflags +faststart \
    #     "$OUTPUT_DIR/${IN_FILE_NAME}.mp4"

ffmpeg -i "$IN_FILE_NAME" \
    -c:v libx264 -preset slow -crf 18 -pix_fmt yuv420p \
    -c:a aac -b:a 192k \
    -movflags +faststart \
    "$OUTPUT_DIR/${IN_FILE_NAME}.mp4"

# convert video stream
# ffmpeg -i "Foundation.2021.S03E01.A.Song.for.the.End.of.Everything.mkv" \
    #     -c:v libx264 -preset slow -crf 18 -pix_fmt yuv420p \
    #     -c:a aac -b:a 192k \
    #     -movflags +faststart \
    #     "Foundation.2021.S03E01.A.Song.for.the.End.of.Everything.mkv.mp4"

# convert subtitles
# ffmpeg -i "Foundation.2021.S03E01.A.Song.for.the.End.of.Everything.mkv" \
    #     -c:v copy \
    #     -c:a aac -b:a 192k \
    #     -movflags +faststart \
    #     "Foundation.2021.S03E01.A.Song.for.the.End.of.Everything.mkv.mp4"
