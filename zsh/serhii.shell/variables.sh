if isMacOs; then
    export PATH="/usr/local/opt/python@3.9/libexec/bin:$PATH"
    export PATH="/usr/local/opt/python/libexec/bin:$PATH"
    export PATH="/Applications/IntelliJ IDEA.app/Contents/MacOS:$PATH"

    # in case commented nvm (as slow)
    export PATH=$PATH:$HOME/.nvm/versions/node/v20.18.0/bin
    export PATH="$PATH:/Applications/Visual Studio Code.app/Contents/Resources/app/bin"
    export PATH="$PATH:/Applications/Sublime Text.app/Contents/SharedSupport/bin/"

    # Add Docker Desktop for Mac (docker)
    export PATH="$PATH:/Applications/Docker.app/Contents/Resources/bin/"

    # iterm:
    # sudo ln -s /Users/serhii/dotfiles/work/bash/iterm.sh /usr/local/bin/iterm

    # tell specific tools where need seek config (lazygit etc)
    export XDG_CONFIG_HOME="$HOME/.config"
else
    # export PATH=$PATH:/home/serhii/homebrew/bin/
    #export PATH=$PATH:/snap/bin
    export PATH=$PATH:$HOME/serhii.home/tools/kafka/bin
    export PATH=$PATH:$HOME/tools/temporal

    # in case commented nvm (as slow)
    export PATH=$PATH:$HOME/.nvm/versions/node/v20.17.0/bin

    # integration between ubuntu\arch (brew/pacman)
    # sudo ln -s /home/linuxbrew/.linuxbrew/bin/yazi /usr/bin/yazi
    # sudo ln -s /home/linuxbrew/.linuxbrew/bin/fd /usr/bin/fd
    # sudo ln -s /home/linuxbrew/.linuxbrew/bin/zellij /usr/bin/zellij
fi

export VISUAL='nvim'
export EDITOR='nvim'

export MANPAGER='nvim +Man!'
# export MANPAGER="sh -c 'col -bx | bat -l man -p'"
export MANWIDTH=999