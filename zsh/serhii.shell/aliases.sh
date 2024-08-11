alias rl='export MANUAL_RL=1 ; exec zsh' # reload shell with variable

# Changing "ls" to "eza"
alias ls='eza -al --color=always --group-directories-first' # my preferred listing
alias la='eza -a --color=always --group-directories-first'  # all files and dirs
alias ll='eza -l --color=always --group-directories-first'  # long format
alias lt='eza -aT --color=always --group-directories-first' # tree listing
alias lh='eza -a | rg --color=always "^\."'
alias l.='eza -al --color=always --group-directories-first ../' # ls on the PARENT directory
alias l..='eza -al --color=always --group-directories-first ../../' # ls on directory 2 levels up
alias l...='eza -al --color=always --group-directories-first ../../../' # ls on directory 3 levels up

# Changing "ps" to "procs"
alias ps="procs"

# alias grep='grep --color=auto'
# Changing "grep" to "repgrep"
alias grep="rg --color=always"

# Changing "find" to "fd"
# defined after sdkman definition

# Changing "cat" to "bat"
#alias cat='bat --decorations=always --color=always' # present issue with ansi
alias cat='bat'

alias yyc='yazi --clear-cache && yy'

# linux
alias ubuntuu="sudo apt update && sudo apt upgrade"
alias ubuntuuf="sudo apt update && sudo apt full-upgrade"
alias ubuntuar="sudo apt autoremove"
alias archu="sudo pacman -Syu"

# navigation
alias ..='cd ..'
alias ...='cd ../..'
alias .3='cd ../../..'
alias .4='cd ../../../..'
alias .5='cd ../../../../..'

alias portsStatus='for device in /sys/bus/usb/devices/*/power/wakeup; do cat "$device"; done'

# changing cd to zoxide
alias cd='z'
alias cdi='zi'

alias work="cd ~/serhii.home/work/"
alias video="cd ~/serhii.home/videos/"
bindkey -s "^w" "work\n"

# ranger with cd to current dir on quit
#alias ranger='ranger --choosedir=$HOME/.rangerdir; LASTDIR=`cat $HOME/.rangerdir`; /usr/bin/zoxide "$LASTDIR"'

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
alias mcc='mvn clean compile'
alias nativeb='mvn clean -Dcyclonedx.skip=true -Djacoco.skip=true -Dmaven.test.skip=true -DskipTests=true -DskipNativeTests -DquickBuild -Pnative native:compile'
alias springbi='mvn clean -Dcyclonedx.skip=true -Djacoco.skip=true -Dmaven.test.skip=true -DskipTests=true spring-boot:build-image'
alias springbi_n='mvn clean -Dcyclonedx.skip=true -Djacoco.skip=true -Dmaven.test.skip=true -DskipTests=true -Pnative spring-boot:build-image'
# ./target/user-service -Dspring.profiles.active=work,default
# java -Dspring.profiles.active=work,default -jar ./target/user-service-local.jar
# export GRAALVM_BUILDTOOLS_MAX_PARALLEL_BUILDS=8
# export APP_CLIENT_SERVICE_AUTH_URL=http://traefik-internal.test/auth/system

alias ideau='intellij-idea-ultimate'
 
alias dup='docker-compose up'
alias ddown='docker-compose down'

alias tmuxan='tmux attach || tmux new -s default \; command-prompt -p "Window name: " "rename-window ''%%''"'
#alias tmuxan='tmux new-session -A -s default'

#alias idea='intellij-idea-ultimate'

#alias tldr='tldr --color=always'
alias short='tldr'

alias git_dotfiles='/usr/bin/git --git-dir=/home/serhii/.dotfiles/ --work-tree=/home/serhii'

alias vim="nvim"
alias vi="nvim"
alias lg="lazygit"

alias fz="fzf --preview='bat {}'"

alias psqlu="sudo -u postgres psql"
alias postgres_start="systemctl start postgresql"
alias postgres_start="systemctl stop postgresql"

alias zj="zellij"
alias q="exit"

alias python="/usr/bin/python3.12"
alias py="/usr/bin/python3.12"

# copy input to buffer depends on OS
if isMacOs; then
    alias buff="pbcopy"
    alias b="buff"
else
    alias buff="xclip clipboard"
    alias b="buff"
fi

# git
alias addup='git add -u'
alias addall='git add .'
alias branch='git branch'
alias checkout='git checkout'
alias clone='git clone'
alias commit='git commit -m'
alias fetch='git fetch'
alias pull='git pull origin'
alias push='git push origin'
alias stat='git status'  # 'status' is protected name so using 'stat' instead
alias tag='git tag'
alias newtag='git tag -a'

# docker
alias dcu='docker-compose up'
alias dcd='docker-compose down'
alias dps='docker ps'

# temporal (localhost:7233, UI http://localhost:8233.)
#alias temporal_start='temporal server start-dev'
alias temporal_start='temporal server start-dev --db-filename temporal_dev_store.db'
