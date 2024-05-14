#!/bin/zsh

sorce ~/.zshrc

if [[ -z "$1" ]]; then
    ( 
    # configs    
    find \
        /home/serhii/ \
        /home/serhii/.local/bin \
         -maxdepth 1 -type f | \
    xargs -I {} printf "nvim {}\nsubl {}\n" \
    ; \
    find \
        /home/serhii/.config/ranger \
        /home/serhii/.config/rofi \
        /home/serhii/.config/qtile \
        /home/serhii/.config/nvim \
        /home/serhii/.config/nitrogen \
        /home/serhii/.config/awesome \
        /home/serhii/.config/alacritty \
         -maxdepth 2 -type f | \
    xargs -I {} printf "nvim {}\nsubl {}\n" \
    ; \

    # books
    #find \
    #    /home/serhii/serhii.home/personal/books

    # work projects    
    find /home/serhii/serhii.home/work/git.work \
	 /home/serhii/serhii.home/personal/git \
        -maxdepth 1 -type d  | xargs -I {} printf "intellij-idea-ultimate {}\n" \
    )
else

    if [[ $1 =~ "nvim" ]]; then
        ( alacritty -e zsh -i -c "$1" > /dev/null 2>&1 & )
    else
        ( eval $1 > /dev/null 2>&1 & )
    fi

    #( alacritty -e zsh -i -c "$1" > /dev/null 2>&1 & )
    #( alacritty -e zsh -i -c "nvim $1" > /dev/null 2>&1 & )
    #( subl "$1" > /dev/null 2>&1 & )
    #( code "$1" > /dev/null 2>&1 & )
    #( /snap/bin/alacritty -e /home/serhii/homebrew/bin//nvim "$1" > /dev/null 2>&1 & )
fi
