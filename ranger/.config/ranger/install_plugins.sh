#!/usr/bin/env bash

RANGER_PLUGINS="$HOME/.config/ranger/plugins"
mkdir -p "$RANGER_PLUGINS"
git clone https://github.com/jchook/ranger-zoxide.git "$RANGER_PLUGINS"/ranger_zoxide
git clone https://github.com/maximtrp/ranger-archives.git "$RANGER_PLUGINS"/ranger-archives
git clone https://github.com/alexanderjeurissen/ranger_devicons "$RANGER_PLUGINS"/ranger_devicons