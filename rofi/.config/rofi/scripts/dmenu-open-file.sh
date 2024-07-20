#!/bin/bash

intellijView="intellij"
vscodeView="vscode"

function replaceToRealIdeCommand() {
    local intellijCmd="intellij-idea-ultimate"
    local vscodeCmd="code"

    local originalCmd="$1"
    originalCmd=$(echo "$originalCmd" | sed "s/$intellijView/$intellijCmd/")
    originalCmd=$(echo "$originalCmd" | sed "s/$vscodeView/$vscodeCmd/")

    echo "$originalCmd"
}

if [[ -z "$1" ]]; then
    (
    # configs
    /usr/bin/fd . --type f --max-depth 1 --hidden --no-follow \
        /home/serhii/ \
        /home/serhii/serhii.shell | \
    xargs -I {} printf "nvim {}\nsubl {}\n" \
    ; \
    /usr/bin/fd . --max-depth 2 --type f --hidden --no-follow \
        /home/serhii/.config/ranger \
        /home/serhii/.config/rofi \
        /home/serhii/.config/qtile \
        /home/serhii/.config/nvim \
        /home/serhii/.config/nitrogen \
        /home/serhii/.config/awesome \
        /home/serhii/.config/alacritty | \
    xargs -I {} printf "nvim {}\nsubl {}\n" \
    ; \
    /usr/bin/fd . --type f --hidden --no-follow \
        /home/serhii/dotfiles | \
    xargs -I {} printf "nvim {}\nsubl {}\n" \
    ; \
    /usr/bin/fd . --type d --max-depth 1 --hidden \
     /home/serhii/serhii.home/work/git.work \
	 /home/serhii/serhii.home/personal/git | \
    xargs -I {} printf "$intellijView {}\n$vscodeView {}\n" \
    )
else
    if [[ $1 =~ "nvim" ]]; then
        ( alacritty -e zsh -i -c "$1" > /dev/null 2>&1 & )
    else

        originalCmd=$(replaceToRealIdeCommand "$1")

        #echo "$originalCmd"
        ( eval "$originalCmd" > /dev/null 2>&1 & )
    fi
fi
