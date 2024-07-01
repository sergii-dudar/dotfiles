if isMacOs; then
    export PATH="/usr/local/opt/python@3.9/libexec/bin:$PATH"
    export PATH="/usr/local/opt/python/libexec/bin:$PATH"
    export PATH="/Applications/IntelliJ IDEA.app/Contents/MacOS:$PATH"

    # Add Docker Desktop for Mac (docker)
    export PATH="$PATH:/Applications/Docker.app/Contents/Resources/bin/"
else
    export PATH=$PATH:/home/serhii/homebrew/bin/
    export PATH=$PATH:/snap/bin
    export PATH=$PATH:/home/serhii/serhii.home/tools/kafka/bin
fi

export VISUAL='nvim'
export EDITOR='nvim'

export MANPAGER='nvim +Man!'
# export MANPAGER="sh -c 'col -bx | bat -l man -p'"
export MANWIDTH=999