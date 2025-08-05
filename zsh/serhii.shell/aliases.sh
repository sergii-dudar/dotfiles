alias rl='export MANUAL_RL=1 ; exec zsh' # reload shell with variable

# short edit
alias dots="cd ~/dotfiles && nvim"

# Changing "ls" to "eza"
alias ls='eza -al --icons --color=always --group-directories-first'
#alias ls2='eza -al --tree --icons --level=2 --color=always --group-directories-first'
# alias la='eza -a --icons --color=always --group-directories-first'  # all files and dirs
# alias ll='eza -l --icons --color=always --group-directories-first'  # long format
# alias lt='eza -aT --icons --color=always --group-directories-first' # tree listing
# alias lh='eza -a | rg --color=always "^\."'
alias lsraw='eza --oneline --color=never --icons=never'
# alias l.='eza -al --icons --color=always --group-directories-first ../'         # ls on the PARENT directory
# alias l..='eza -al --icons --color=always --group-directories-first ../../'     # ls on directory 2 levels up
# alias l...='eza -al --icons --color=always --group-directories-first ../../../' # ls on directory 3 levels up

alias music="rmpc" # "ncmpcpp"

alias yaziu="cargo install --locked yazi-fm yazi-cli"
if isMacOs; then
    # https://github.com/FelixKratz/JankyBorders
    alias run_borders="borders active_color=0xffa3be8c inactive_color=0xff494d64 width=5.0 &"
    alias brewu="brew update && brew upgrade"
    alias brewua="brew update && brew upgrade && yaziu"
    alias brave="/Applications/Brave\ Browser.app/Contents/MacOS/Brave\ Browser"
    alias move_cursor='bash -c "cd ~/dotfiles/work/python && pipenv run python mouse_cursor.py > /dev/null 2>&1"'
else
    alias amenu="~/.config/rofi/scripts/launcher_t1"
    alias pmenu="~/.config/rofi/scripts/powermenu_t1"
    alias wifimenu="~/.config/rofi/scripts/wifimenu"

    alias collect_packages="~/dotfiles/arch/packages-sync/collect-packages.sh"
    alias sync_packages="~/dotfiles/arch/packages-sync/sync-packages.sh"

    alias portsStatus='for device in /sys/bus/usb/devices/*/power/wakeup; do cat "$device"; done'

    alias ubuntuu="sudo apt update && sudo apt upgrade"
    alias ubuntuuf="sudo apt update && sudo apt full-upgrade"
    alias ubuntuar="sudo apt autoremove"
    alias archu="sudo pacman -Syu && yay -Syu"
    alias archua="archu && yaziu"
fi

# Changing "ps" to "procs"
alias ps="procs"

# alias grep='grep --color=auto'
# Changing "grep" to "repgrep"
# alias grep="rg --color=always"
alias rg="rg --color=always"

# Changing "find" to "fd"
# defined after sdkman definition

# Changing "cat" to "bat"
#alias cat='bat --decorations=always --color=always' # present issue with ansi
alias cat='bat'

alias yyc='yazi --clear-cache && yy'

# java
alias mvn_sources="mvn dependency:sources && mvn dependency:resolve -Dclassifier=javadoc"

# navigation
alias ..='cd ..'
alias ...='cd ../..'
alias .3='cd ../../..'
alias .4='cd ../../../..'
alias .5='cd ../../../../..'

# changing cd to zoxide
alias cd='z'
alias cdi='zi'

# alias tmuxan='tmux attach || tmux new' # connect to exists session or create new one
# alias tmuxan='tmux attach || tmux new -s ''default'' -n ''ranger.h'' ''ranger .'''

# alias tmuxan='tmux attach || tmux new -s default \; command-prompt -p "Window name: " "rename-window ''%%''"'
#alias tmuxan='tmux new-session -A -s default'

#alias idea='intellij-idea-ultimate'

#alias tldr='tldr --color=always'
alias short='tldr'

alias vim="nvim"
alias vi="nvim"
alias vit="nvim -p"
alias lg="lazygit"

# alias fz="fzf --preview='bat {}'"

# alias psqlu="sudo -u postgres psql"
# alias postgres_start="systemctl start postgresql"
# alias postgres_start="systemctl stop postgresql"

# alias zj="zellij"
# alias q="exit"

#alias python="/usr/bin/python3.12"
#alias py="/usr/bin/python3.12"

# copy input to buffer depends on OS
# if isMacOs; then
#     alias buff="pbcopy"
#     alias b="buff"
# else
#     alias buff="xclip clipboard"
#     alias b="buff"
# fi

# git
# alias addup='git add -u'
# alias addall='git add .'
# alias branch='git branch'
# alias checkout='git checkout'
# alias clone='git clone'
# alias commit='git commit -m'
# alias fetch='git fetch'
# alias pull='git pull origin'
# alias push='git push origin'
# alias stat='git status'  # 'status' is protected name so using 'stat' instead
# alias tag='git tag'
# alias newtag='git tag -a'

# tmux
alias tmuxkill='tmux kill-server'

# rust
# alias rustdoc='rustup doc'
