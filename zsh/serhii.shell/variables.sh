# tell specific tools where need seek config (lazygit, aerospace etc)
export XDG_CONFIG_HOME="$HOME/.config"

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

    alias setwalls="~/dotfiles/bin/macos/set_wallpapers.sh"
    function kanata_run() {
        sudo bash -c "$(which kanata) --cfg $HOME/.config/kanata/kanata.kbd >> /var/log/kanata.log 2>&1 &"
    }

    # iterm:
    # sudo ln -s /Users/serhii/dotfiles/work/bash/iterm.sh /usr/local/bin/iterm

    # Rust Cargo (Pre):
    # In general to use latest app versions (like yazi), as brew getting latest with big delay, while arch already have it.
    # to use from brew, just comment it.
    # cargo install --locked yazi-fm yazi-cli
    export PATH=$HOME/.cargo/bin:$PATH

    # Haskell
    export PATH=$PATH:$HOME/.ghcup/bin
    #export PATH=$HOME/.ghcup/bin:$PATH

    #export REQUESTS_CA_BUNDLE=$(python3 -m certifi)

    # dotnet
    # export DOTNET_ROOT="$(dirname $(which dotnet))"
else

    alias out="sudo pkill -KILL -u serhii"
    # alias window_type="xprop | grep -i wm_class"
    alias window_type="xprop WM_CLASS WM_NAME"

    # export PATH=$PATH:/home/serhii/homebrew/bin/
    #export PATH=$PATH:/snap/bin
    export PATH=$PATH:$HOME/serhii.home/tools/kafka/bin
    export PATH=$PATH:$HOME/tools/temporal

    # in case commented nvm (as slow)
    # nvm install --lts
    # export PATH=$HOME/.nvm/versions/node/v20.17.0/bin:$PATH
    export PATH=$HOME/.nvm/versions/node/v24.11.0/bin:$PATH
    #export PATH=$HOME/.ghcup/bin:$PATH
    export PATH=$PATH:$HOME/.ghcup/bin
    export PATH=$HOME/.cargo/bin:$PATH
    export PATH=$PATH:$HOME/.local/bin

    export QT_SCALE_FACTOR=1.75
    # export QT_SCREEN_SCALE_FACTORS=1.75
    export LC_CTYPE=en_US.UTF-8

    # LC_CTYPE=en_US.UTF-8 > /etc/locale.conf
    # integration between ubuntu\arch (brew/pacman)
    # sudo ln -s /home/linuxbrew/.linuxbrew/bin/yazi /usr/bin/yazi
    # sudo ln -s /home/linuxbrew/.linuxbrew/bin/fd /usr/bin/fd
    # sudo ln -s /home/linuxbrew/.linuxbrew/bin/zellij /usr/bin/zellij

    # debug qtile wm
    alias qtile_debug="tail -f ~/.local/share/qtile/qtile.log"

    # debug awesome wm
    # sudo pacman -S xorg-server-xephyr
    alias awesome_debug="Xephyr :5 -screen 1920x1080 & sleep 1 ; DISPLAY=:5 awesome"

    # dotnet
    export DOTNET_ROOT=$HOME/.dotnet


    case "$XDG_SESSION_TYPE" in
        wayland)
            # echo "Running on Wayland"
            ;;
        x11)
            # 200 → Delay before key repeat starts (in milliseconds).
            # 50 → Repeat rate (keys per second).
            xset r rate 200 30
            ;;
        *)
            # echo "Unknown session type"
            ;;
    esac
fi

export VISUAL='nvim'
export EDITOR='nvim'

export MANPAGER='nvim +Man!'
# export MANPAGER="sh -c 'col -bx | bat -l man -p'"
export MANWIDTH=999

# enable Go modules to install the packages
export GO111MODULE=on