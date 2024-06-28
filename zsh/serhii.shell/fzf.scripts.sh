# Set up fzf key bindings and fuzzy completion

if [ "$(command -v fzf)" ]; then
  # Set up fzf key bindings and fuzzy completion
  eval "$(fzf --zsh)"
fi

# - ctrl + r to find command from zsh history

HISTFILE=~/.zsh_history
HISTSIZE=50000
SAVEHIST=10000
setopt appendhistory

# ===============================
# ===============Fuzzy completion
# https://github.com/junegunn/fzf?tab=readme-ov-file#files-and-directories
# vim **<TAB>               - Files under the current directory
# vim ../**<TAB>            - Files under parent directory
# vim ../fzf**<TAB>         - Files under parent directory that match `fzf`
# vim ~/**<TAB>             - Files under your home directory
#
# cd **<TAB>                - Directories under current directory (single-selection)
# cd ~/github/fzf**<TAB>    - Directories under ~/github that match `fzf`
#
# kill -9 **<TAB>           - Fuzzy completion for PIDs is provided for kill command
# Can select multiple processes with <TAB> or <Shift-TAB> keys
#


# ===============================
# =============== Bindings

# Default command & options to use when input is tty
export FZF_DEFAULT_COMMAND='fd --type f --color=always --hidden --exclude .git'
export FZF_DEFAULT_OPTS="--ansi --info=inline --height 80% --layout reverse --border"

# ===== CTRL-T runs $FZF_CTRL_T_COMMAND to get a list of files and directories
export FZF_CTRL_T_COMMAND='fd --color=always --hidden --exclude .git'
export FZF_CTRL_T_OPTS="
--walker-skip .git,node_modules,target
--preview 'bat --color=always --line-range :50 {}'
--bind 'ctrl-/:change-preview-window(down|hidden|)'"

# ===== CTRL-R - Paste the selected command from history onto the command-line
# CTRL-/ to toggle small preview window to see the full command
# CTRL-Y to copy the command into clipboard using pbcopy
export FZF_CTRL_R_OPTS="
  --preview 'echo {}' --preview-window up:3:hidden:wrap
  --bind 'ctrl-/:toggle-preview'
  --bind 'ctrl-y:execute-silent(echo -n {2..} | pbcopy)+abort'
  --color header:italic
  --header 'Press CTRL-Y to copy command into clipboard'"

# ===== ALT-C runs $FZF_ALT_C_COMMAND to get a list of directories
export FZF_ALT_C_COMMAND='fd --type d --color=always --hidden --exclude .git'
# Print tree structure in the preview window
export FZF_ALT_C_OPTS="
  --walker-skip .git,node_modules,target
  --preview 'tree -C -L 1 {}'"

if isMacOs; then
    # workaround for fzf keybinding with alt+
    bindkey "ç" fzf-cd-widget
    bindkey "†" fzf-file-widget
    bindkey "®" fzf-history-widget
fi

# function reread_zshrc() {
#    echo 'Hello bind'
#}
#zle -N reread_zshrc
#bindkey "ƒ" reread_zshrc

# ==================================================
# ================= searching file content \ replace

alias grepid="grepf idea";
alias grepco="grepf code";
alias grepvi="grepf nvim";
alias grepsu="grepf subl";

function grepf() {
    local editor="${1:-nvim}"
    shift
    # echo $editor

    rg --color=always --line-number --no-heading --smart-case "${*:-}" "$PWD" |
      fzf --ansi \
          --exact \
          --color "hl:-1:underline,hl+:-1:underline:reverse" \
          --delimiter : \
          --preview 'bat --color=always {1} --highlight-line {2}' \
          --preview-window 'up,60%,border-bottom,+{2}+3/3,~3' \
          --bind "enter:become($editor {1})"
}

function findt() {
    if [[ "$1" == "--help" || "$1" == "-h" ]]; then
        cat << EOF
find text in files from currect dir: findt [text]
EOF
        return 0
    fi

    egrep -ir "($1)" .
}

function findt_in() {
    if [[ "$1" == "--help" || "$1" == "-h" ]]; then
        cat << EOF
find text in files from currect dir: findt [text] [*.yaml]
EOF
        return 0
    fi

    grep -r -n -i --include="$2" "$1" .
}

function findt_in_r() {
	find . -name $2 -exec sed -i "s/$1/$3/g" {} \;
}

# ============================
# =================== testing

# fkill - kill process
fkill() {
  # with ability to updating the list of processes by pressing CTRL-R

  (date; ps -ef) |
    fzf --exact --bind='ctrl-r:reload(date; ps -ef)' \
        --header=$'Press CTRL-R to reload\n\n' --header-lines=2 \
        --preview='echo {}' --preview-window=down,5,wrap \
        --layout=reverse --height=80% | awk '{print $2}' | xargs kill -9
}

ffind() {
    fd --type file |
      fzf --exact --prompt 'Files> ' \
          --header 'CTRL-T: Switch between Files/Directories' \
          --bind 'ctrl-t:transform:[[ ! $FZF_PROMPT =~ Files ]] &&
                  echo "change-prompt(Files> )+reload(fd --type file)" ||
                  echo "change-prompt(Directories> )+reload(fd --type directory)"' \
          --preview '[[ $FZF_PROMPT =~ Files ]] && bat --color=always {} || tree -C -L 1 {}'
}

# ==============================================
# ======================= find and edit by EDITOR

function fze() {
    result=$(fzf --exact --ansi --info=inline --height 80% --layout reverse --border --preview='bat --color=always --style=numbers --line-range=:500 {}')
    if [ -n "$result" ]; then
        $EDITOR $result
    fi
}

function fzc() {
     result=$( ( \
        fd . --type f --color=always --hidden --exclude .git \
            /home/serhii/ \
            /home/serhii/.local/bin \
             --max-depth 1 \
        ; \
        fd . --type f --color=always --hidden --exclude .git \
            /home/serhii/.config/ranger \
            /home/serhii/.config/rofi \
            /home/serhii/.config/qtile \
            /home/serhii/.config/nvim \
            /home/serhii/.config/nitrogen \
            /home/serhii/.config/awesome \
            /home/serhii/.config/alacritty \
            /etc/keyd \
             --max-depth 2 \
        ; \
        fd . --type d --color=always --hidden --exclude .git /home/serhii/serhii.home/work/git.work \
            --max-depth 1 ) | ( fzf --exact --ansi --info=inline --height 80% --layout reverse --border --preview='bat --color=always --style=numbers --line-range=:500 {}' ) )

        if [ -n "$result" ]; then
            $EDITOR $result
        fi
}
