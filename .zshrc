# If you come from bash you might have to change your $PATH.
# export PATH=$HOME/bin:/usr/local/bin:$PATH

# Path to your oh-my-zsh installation.
export ZSH="$HOME/.oh-my-zsh"

# Set name of the theme to load --- if set to "random", it will
# load a random theme each time oh-my-zsh is loaded, in which case,
# to know which specific one was loaded, run: echo $RANDOM_THEME
# See https://github.com/ohmyzsh/ohmyzsh/wiki/Themes

#ZSH_THEME="robbyrussell"
ZSH_THEME="simple"

# Set list of themes to pick from when loading at random
# Setting this variable when ZSH_THEME=random will cause zsh to load
# a theme from this variable instead of looking in $ZSH/themes/
# If set to an empty array, this variable will have no effect.
# ZSH_THEME_RANDOM_CANDIDATES=( "robbyrussell" "agnoster" )

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to use hyphen-insensitive completion.
# Case-sensitive completion must be off. _ and - will be interchangeable.
# HYPHEN_INSENSITIVE="true"

# Uncomment one of the following lines to change the auto-update behavior
# zstyle ':omz:update' mode disabled  # disable automatic updates
# zstyle ':omz:update' mode auto      # update automatically without asking
# zstyle ':omz:update' mode reminder  # just remind me to update when it's time

# Uncomment the following line to change how often to auto-update (in days).
# zstyle ':omz:update' frequency 13

# Uncomment the following line if pasting URLs and other text is messed up.
# DISABLE_MAGIC_FUNCTIONS="true"

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
# ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
# You can also set it to another string to have that shown instead of the default red dots.
# e.g. COMPLETION_WAITING_DOTS="%F{yellow}waiting...%f"
# Caution: this setting can cause issues with multiline prompts in zsh < 5.7.1 (see #5765)
# COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# You can set one of the optional three formats:
# "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# or set a custom format using the strftime function format specifications,
# see 'man strftime' for details.
# HIST_STAMPS="mm/dd/yyyy"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Which plugins would you like to load?
# Standard plugins can be found in $ZSH/plugins/
# Custom plugins may be added to $ZSH_CUSTOM/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(
    git
    vi-mode
    sdk
    tmux
    # mvn
    # kubectl
    # gradle
    #dotenv
    #zsh-autosuggestions
    zsh-syntax-highlighting
    )

# vi-mode
# https://github.com/ohmyzsh/ohmyzsh/blob/master/plugins/vi-mode/README.md
#VI_MODE_RESET_PROMPT_ON_MODE_CHANGE=true
VI_MODE_SET_CURSOR=true
MODE_INDICATOR="[%F{yellow}normal%f]"
#INSERT_MODE_INDICATOR="%F{yellow}INSERT%f"
VI_MODE_DISABLE_CLIPBOARD=false

# zsh-autosuggestions
# git clone https://github.com/zsh-users/zsh-autosuggestions ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-autosuggestions

# zsh-syntax-highlighting
# git clone https://github.com/zsh-users/zsh-syntax-highlighting.git ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-syntax-highlighting


source $ZSH/oh-my-zsh.sh

# User configuration

# export MANPATH="/usr/local/man:$MANPATH"

# You may need to manually set your language environment
# export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
# if [[ -n $SSH_CONNECTION ]]; then
#   export EDITOR='vim'
# else
#   export EDITOR='mvim'
# fi

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.
#
# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"

# Define ANSI escape codes for colors
BLACK='\033[0;30m'
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
BLUE='\033[0;34m'
MAGENTA='\033[0;35m'
CYAN='\033[0;36m'
BOLD='\033[1m'
BOLD_BLUE='\033[1;34m'
BOLD_YELLOW='\033[1;33m'
UNDERLINE='\033[4m'
RESET='\033[0m' # Reset color to default

alias rl='exec zsh' # reload shell 

alias l='ls -l'         # ls with items and directory details
alias la='ls -a'        # ls all items and directories within cd
alias lA='ls -A'        # ls all items and directories within cd, EXCEPT "." and ".."
alias lla='ls -la'      # combines "ls -l" and "ls -a"
alias llA='ls -lA'      # combines "ls -l" and "ls -A"

alias az_dev='az aks get-credentials --resource-group dev --name aks-dev'
alias az_qa='az aks get-credentials --resource-group qa --name aks-qa'
alias az_uat='az aks get-credentials --resource-group uat --name aks-uat'

alias k9sa='k9s -n all'

alias k9sd='az_dev ; k9sa'
alias k9sq='az_qa ; k9sa'
alias k9su='az_uat ; k9sa'

alias msa='mvn spotless:apply'
alias helmdu='helm dependency update'
alias helmu='cd helm; helmdu'

alias gitreset_comit='git reset HEAD~'

alias tmuxan='tmux attach || tmux new' # connect to exists session or create new one
# alias tmuxan='tmux attach || tmux new -s ''default'' -n ''ranger.h'' ''ranger .'''

alias mvncc='msa ; mvn clean compile'
alias mvncv='msa ; mvn clean verify'

alias ideau='intellij-idea-ultimate'
 
alias dup='docker-compose up'
alias ddown='docker-compose down'

alias tmuxan='tmux attach || tmux new -s default \; command-prompt -p "Window name: " "rename-window ''%%''"'
#alias tmuxan='tmux new-session -A -s default'

#alias idea='intellij-idea-ultimate'

alias short='tldr'
 
alias git_dotfiles='/usr/bin/git --git-dir=/home/serhii/.dotfiles/ --work-tree=/home/serhii'

alias vim="nvim"
alias vi="nvim"
alias lg="lazygit"

alias fz="fzf --preview='cat {}'"
function fze() {
    result=$(fzf --preview='cat {}')
    if [ -n "$result" ]; then
        $EDITOR $result
    fi
}

function fzc() {
     result=$( ( \
        find \
            /home/serhii/ \
            /home/serhii/.local/bin \
             -maxdepth 1 -type f \
        ; \
        find \
            /home/serhii/.config/ranger \
            /home/serhii/.config/rofi \
            /home/serhii/.config/qtile \
            /home/serhii/.config/nvim \
            /home/serhii/.config/nitrogen \
            /home/serhii/.config/awesome \
            /home/serhii/.config/alacritty \
            /etc/keyd \
             -maxdepth 2 -type f \
        ; \
        find /home/serhii/serhii.home/work/git.work \
            -maxdepth 1 -type d ) | ( fzf --preview='cat {}' ) )

        if [ -n "$result" ]; then
            $EDITOR $result
        fi
}

function getLogLevel() {
    ports=(${(@s:,:)1})
    for port in "${ports[@]}"; do
            curl --silent --request GET \
                 --url "http://localhost:$port/actuator/loggers/$2"
    done
}

function changeLogLevel() {
    ports=(${(@s:,:)1})
    for port in "${ports[@]}"; do

            curl --request POST \
                --url "http://localhost:$port/actuator/loggers/$2" \
                --header 'Content-Type: application/json' \
                --data "{
                \"configuredLevel\": \"$3\"
                }"
            echo -e ">>>> $port: Level to package: ${BOLD_BLUE}\"$2\"${RESET} successfully change to ${BOLD_YELLOW}\"$3\"${RESET}"
            echo -e ">>>> $port: Actual level of package: ${BOLD_BLUE}\"$2\"${RESET} is: ${BOLD_YELLOW}$(getLogLevel $port "$2")${RESET}"
    done
}

function getLogLevel8091() {
    getLogLevel 8091 "$1"
}

function changeLogLevel8091() {
    changeLogLevel 8091 "$1" "$2"
}

function chrome() {
	google-chrome > /dev/null 2>&1 &
}

function findt() { 
    egrep -ir "($1)" .
}    

function findt_in() {
    grep -r -n -i --include="$2" "$1" .
}

function findt_in_r() {
	find . -name $2 -exec sed -i "s/$1/$3/g" {} \;
}

function idea() {
    intellij-idea-ultimate "$1" > /dev/null 2>&1 &
}

function open_terminal() {
   alacritty
}

function files() {
    nautilus "$1" > /dev/null 2>&1 &
}

function s_restart() {
    sudo systemctl restart "$1"
}

function s_status() {
    sudo systemctl status "$1"
}

function copy_content() {
    xclip -selection clipboard $1
}

function topCommands() {
    history | awk 'BEGIN {FS="[ \t]+|\\|"} {print $3}' | sort | uniq -c | sort -nr | head -$1
}

#function copy_file() {
#    # mac
#    # brew install findutils
#    # brew install coreutils
#      osascript -e{'on run{a}','set the clipboard to posix file a',end} "$(greadlink -f -- "$1")"
#
#      # linux
#      # ...
#}

#export HOMEBREW_FORCE_BREWED_CURL=1
export PATH=$PATH:/home/serhii/homebrew/bin/
#export PATH=$PATH:/snap/intellij-idea-ultimate/current/bin
export PATH=$PATH:/snap/bin
#export PATH=$PATH:/usr/bin/python3
#export PATH=$HOME/.cargo/env
#export PATH=$HOME/.cargo/bin

export VISUAL='nvim'
export EDITOR='nvim'

export MANPAGER='nvim +Man!'
export MANWIDTH=999

#THIS MUST BE AT THE END OF THE FILE FOR SDKMAN TO WORK!!!
export SDKMAN_DIR="$HOME/.sdkman"
[[ -s "$HOME/.sdkman/bin/sdkman-init.sh" ]] && source "$HOME/.sdkman/bin/sdkman-init.sh"

# Set up fzf key bindings and fuzzy completion
eval "$(fzf --zsh)"

export NVM_DIR="$HOME/.config/nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion
