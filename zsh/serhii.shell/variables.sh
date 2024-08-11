if isMacOs; then
    export PATH="/usr/local/opt/python@3.9/libexec/bin:$PATH"
    export PATH="/usr/local/opt/python/libexec/bin:$PATH"
    export PATH="/Applications/IntelliJ IDEA.app/Contents/MacOS:$PATH"

    # Add Docker Desktop for Mac (docker)
    export PATH="$PATH:/Applications/Docker.app/Contents/Resources/bin/"
else
    export PATH=$PATH:/home/serhii/homebrew/bin/
    #export PATH=$PATH:/snap/bin
    export PATH=$PATH:/home/serhii/serhii.home/tools/kafka/bin
    export PATH=$PATH:/home/serhii/tools/temporal

    # sudo ln -s /home/serhii/homebrew/bin//yazi /usr/local/bin/yazi
    # sudo ln -s /home/serhii/homebrew/bin//yazi /usr/bin/yazi

    # sudo ln -s /home/serhii/homebrew/bin//fd /usr/local/bin/fd
    # sudo ln -s /home/serhii/homebrew/bin//bat /usr/local/bin/bat
    # sudo ln -s /home/serhii/homebrew/bin//fastfetch /usr/local/bin/fastfetch
    # sudo ln -s /home/serhii/homebrew/bin//rg /usr/local/bin/rg
    # sudo ln -s /home/serhii/homebrew/bin//nvim /usr/local/bin/nvim
    # sudo ln -s /home/serhii/homebrew/bin//eza /usr/local/bin/eza
    # sudo ln -s /home/serhii/homebrew/bin//k9s /usr/local/bin/k9s
    # sudo ln -s /home/serhii/homebrew/bin//tldr /usr/local/bin/tldr
    # sudo ln -s /home/serhii/homebrew/bin//fzf /usr/local/bin/fzf
    # sudo ln -s /home/serhii/homebrew/bin//zellij /usr/local/bin/zellij
    # sudo ln -s /home/serhii/homebrew/bin//procs /usr/local/bin/procs

    # to rofi: sudo ln -s /home/serhii/homebrew/bin//fd /usr/bin/fd
fi

export VISUAL='nvim'
export EDITOR='nvim'

export MANPAGER='nvim +Man!'
# export MANPAGER="sh -c 'col -bx | bat -l man -p'"
export MANWIDTH=999