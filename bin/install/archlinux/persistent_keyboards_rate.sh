#!/usr/bin/env bash

sudo nvim /etc/X11/xorg.conf.d/00-keyboard.conf

# Section "InputClass"
#         ...
#         Option "AutoRepeat" "200 30" # <-- add this
#         ...
# EndSection