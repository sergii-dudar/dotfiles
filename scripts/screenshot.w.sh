#!/usr/bin/env bash

grim -g "$(slurp)" - | swappy -f -

# same, with save screenchot to file
# mkdir -p ~/Pictures/Screenshots && FILE=~/Pictures/Screenshots/screenshot-$(date +%Y%m%d-%H%M%S).png && grim -g "$(slurp)" "$FILE" && swappy -f "$FILE"

