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
        ~/ \
        ~/serhii.shell | \
    xargs -I {} printf "nvim {}\nsubl {}\n" \
    ; \
    /usr/bin/fd . --max-depth 2 --type f --no-follow \
        ~/.config/ranger \
        ~/.config/rofi \
        ~/.config/qtile \
        ~/.config/nvim \
        ~/.config/nitrogen \
        ~/.config/awesome \
        ~/.config/alacritty | \
    xargs -I {} printf "nvim {}\nsubl {}\n" \
    ; \
    /usr/bin/fd . --type f --no-follow \
        ~/dotfiles | \
    xargs -I {} printf "nvim {}\nsubl {}\n" \
    ; \
    /usr/bin/fd . --type d --max-depth 1 \
     ~/serhii.home/work/git.work \
	 ~/serhii.home/personal/git | \
    xargs -I {} printf "$intellijView {}\n$vscodeView {}\n" \
    )
else
    if [[ $1 =~ "nvim" ]]; then
        #( alacritty -e zsh -i -c "$1" > /dev/null 2>&1 & )
        ( kitty --hold zsh -c "$1" > /dev/null 2>&1 & )
    else

        originalCmd=$(replaceToRealIdeCommand "$1")

        #echo "$originalCmd"
        ( eval "$originalCmd" > /dev/null 2>&1 & )
    fi
fi
