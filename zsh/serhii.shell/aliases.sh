alias rl='exec zsh' # reload shell 

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

# navigation
alias ..='cd ..'
alias ...='cd ../..'
alias .3='cd ../../..'
alias .4='cd ../../../..'
alias .5='cd ../../../../..'

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
alias nativeb='time ./mvnw clean -Dcyclonedx.skip=true -Djacoco.skip=true -Dmaven.test.skip=true -DskipTests=true -DskipNativeTests -DquickBuild -Pnative native:compile'
alias springbi='mvn clean -Dcyclonedx.skip=true -Djacoco.skip=true -Dmaven.test.skip=true -DskipTests=true spring-boot:build-image'
alias springbi_n='mvn clean -Dcyclonedx.skip=true -Djacoco.skip=true -Dmaven.test.skip=true -DskipTests=true -Pnative spring-boot:build-image'

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

alias fz="fzf --preview='bat {}'"

alias psqlu="sudo -u postgres psql"
alias postgres_start="systemctl start postgresql"
alias postgres_start="systemctl stop postgresql"

alias zj="zellij"
alias q="exit"

alias python="/usr/bin/python3.12"
alias py="/usr/bin/python3.12"

alias cat='bat'


