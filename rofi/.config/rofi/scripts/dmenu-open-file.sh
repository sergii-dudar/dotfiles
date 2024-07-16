#!/bin/bash

intellijView="intellij"
vscodeView="vscode"

typeset -A nameToCommandMap
nameToCommandMap[$intellijView]="intellij-idea-ultimate"
nameToCommandMap[$vscodeView]="code"

funciton getMapValueOrKey() {
    local key="$1"
    local value="${nameToCommandMap[$key]}"
    if [ -z "$value" ]; then
        echo "$key"
    else
        echo "$value"
    fi
}

if [[ -z "$1" ]]; then
    ( 
    # configs    
    /usr/bin/fd . --type f --max-depth 1 --hidden --no-follow \
        /home/serhii/ \
        /home/serhii/serhii.shell | \
        #/home/serhii/.local/bin | \
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

        originalCmd=$1
        #for key in "${!nameToCommandMap[@]}"; do
        #    originalCmd=$(echo "$originalCmd" | sed "s/$key/${nameToCommandMap[$key]}/")
        #done

        for key in "${!nameToCommandMap[@]}"; do
            originalCmd=$(echo "$originalCmd" | sed "s/$key/${nameToCommandMap[$key]}/")
        done

        ( eval "$originalCmd" > /dev/null 2>&1 & )
    fi

    #( alacritty -e zsh -i -c "$1" > /dev/null 2>&1 & )
    #( alacritty -e zsh -i -c "nvim $1" > /dev/null 2>&1 & )
    #( subl "$1" > /dev/null 2>&1 & )
    #( code "$1" > /dev/null 2>&1 & )
    #( /snap/bin/alacritty -e /home/serhii/homebrew/bin//nvim "$1" > /dev/null 2>&1 & )
fi
