#!/usr/bin/env bash

# reference https://github.com/adi1090x/rofi.git

Color_Off='\033[0m'
BBlack='\033[1;30m' BRed='\033[1;31m'    BGreen='\033[1;32m' BYellow='\033[1;33m'
BBlue='\033[1;34m'  BPurple='\033[1;35m' BCyan='\033[1;36m'  BWhite='\033[1;37m'

DIR=$(pwd)
FONT_DIR="$HOME/.local/share/fonts"

echo -e "${BBlue}""\n[*] Installing fonts..." "${Color_Off}"
if [[ -d "$FONT_DIR" ]]; then
    cp -rf "$DIR"/fonts/* "$FONT_DIR"
else
    mkdir -p "$FONT_DIR"
    cp -rf "$DIR"/fonts/* "$FONT_DIR"
fi
echo -e "${BYellow}""[*] Updating font cache...\n" "${Color_Off}"
fc-cache

