# Set up fzf key bindings and fuzzy completion
# https://github.com/junegunn/fzf/blob/master/ADVANCED.md

if [ "$(command -v fzf)" ]; then
    # Set up fzf key bindings and fuzzy completion
    eval "$(fzf --zsh)"
fi

# - ctrl + r to find command from zsh history

HISTFILE=~/.zsh_history
HISTSIZE=50000
SAVEHIST=10000
setopt appendhistory

b=$(tput bold)
n=$(tput sgr0)
s=$'\033[35m'
k=$'\033[32m'

sep="${s}${n}"
function _klabel() {
    echo "[${b}${k}$1${n}]:"
}
function _klabels() {
    echo " ${sep} [${b}${k}$1${n}]:"
}

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

# color picker: https://minsw.github.io/fzf-color-picker/
# morhetz/gruvbox
#fzf_colors='bg+:#3c3836,bg:#32302f,spinner:#81A1C1,hl:#928374,fg:#ebdbb2,header:#928374,info:#8ec07c,pointer:#fb4934,marker:#fb4934,fg+:#ebdbb2,prompt:#81A1C1,hl+:#fb4934'
# junegunn/seoul256.vim (dark)
#fzf_colors='bg+:#3F3F3F,bg:#4B4B4B,border:#6B6B6B,spinner:#98BC99,hl:#719872,fg:#D9D9D9,header:#719872,info:#BDBB72,pointer:#E12672,marker:#E17899,fg+:#D9D9D9,preview-bg:#3F3F3F,prompt:#98BEDE,hl+:#98BC99'
# arcticicestudio/nord-vim
#fzf_colors='bg+:#3B4252,bg:#2E3440,spinner:#81A1C1,hl:#616E88,fg:#D8DEE9,header:#616E88,info:#81A1C1,pointer:#81A1C1,marker:#81A1C1,fg+:#D8DEE9,prompt:#81A1C1,hl+:#81A1C1'
# tomasr/molokai
#fzf_colors='bg+:#293739,bg:#1B1D1E,border:#808080,spinner:#E6DB74,hl:#7E8E91,fg:#F8F8F2,header:#7E8E91,info:#A6E22E,pointer:#A6E22E,marker:#F92672,fg+:#F8F8F2,prompt:#F92672,hl+:#F92672'

# custom
fzf_colors='bg+:#3c3836,spinner:#81A1C1,hl:#928374,fg:#ebdbb2,header:#928374,info:#8ec07c,pointer:#fb4934,marker:#fb4934,fg+:#ebdbb2,prompt:#81A1C1,hl+:#fb4934'

# Default command & options to use when input is tty
export FZF_DEFAULT_COMMAND='fd --type f --color=always --hidden --exclude .git'
export FZF_DEFAULT_OPTS="
--header-first
--exact
--pointer '󰁕'
--preview-border=none
--color=$fzf_colors,header:italic
--ansi --info=inline --height 100% --layout reverse
--border --style minimal
--highlight-line --cycle --wrap-word
--prompt='❯ ' --info=inline-right --no-separator
--bind 'ctrl-h:toggle-preview'
--bind 'ctrl-/:change-preview-window(down|)'"

# ===== CTRL-T runs $FZF_CTRL_T_COMMAND to get a list of files and directories
# --no-ignore
export FZF_CTRL_T_COMMAND='fd --type file --color=always --hidden --exclude .git --exclude node_modules'
_fzf_crtl_t_header=" $(_klabel '󰘴y')Copy  $(_klabels 'enter')Apply "
export FZF_CTRL_T_OPTS=$'
--prompt \' Files ❯ \'
--header \''"${_fzf_crtl_t_header}"$'\'
--preview-window \'right,60%\'
--bind \'enter:execute(LIMITED=Y nvim {})+abort\'
--bind \'ctrl-s:execute(subl {} &)+abort\'
--bind \'ctrl-c:execute(cd {})+abort\'
--preview \'bat --style=changes --color=always {}\''
# --walker-skip .git,node_modules,target,bin


# ===== CTRL-R - Paste the selected command from history onto the command-line
# CTRL-/ to toggle small preview window to see the full command
# CTRL-Y to copy the command into clipboard using pbcopy
_clip=$(command -v pbcopy || command -v wl-copy || echo "xclip -selection clipboard")
_fzf_crtl_r_header=" $(_klabel '󰘴y')Copy  $(_klabels 'enter')Apply "
export FZF_CTRL_R_OPTS=$'
--prompt \'  Cmd History ❯ \'
--header \''"${_fzf_crtl_r_header}"$'\'
--preview \'echo {}\'
--preview-window up:3:hidden:wrap
--bind \'ctrl-y:execute-silent(echo -n {2..} | \''"${_clip}"$'\')+abort\''

# ===== ALT-C runs $FZF_ALT_C_COMMAND to get a list of directories
# export FZF_ALT_C_COMMAND='fd --type d --color=always --hidden --exclude .git'
export FZF_ALT_C_COMMAND='zoxide query -l'
_fzf_alt_c_header=" $(_klabel '󰘴t')Switch (Z 🚀/Dirs 🔎)$(_klabels '󰘴e')Nvim  $(_klabels '󰘴i')Idea $(_klabels '󰘴y')Yazi 📁$(_klabels '󰘴g')Grept 🔭$(_klabels 'enter')CD 󰿄"
export FZF_ALT_C_OPTS=$'
    --exact
    --prompt \'🚀 Zoxide ❯ \'
    --header \''"${_fzf_alt_c_header}"$'\'
    --border-label \' Directories Manager \'
    --bind \'ctrl-t:transform:[[ ! $FZF_PROMPT =~ Zoxide ]] &&
echo "change-prompt(🚀 Zoxide ❯ )+reload(zoxide query -l)" ||
echo "change-prompt(🔎 Dirs ❯ )+reload(fd . --type directory --hidden --exclude .git --exclude target --exclude bin $HOME)"\'
--bind \'ctrl-e:become(cd {} && nvim)\'
--bind \'ctrl-y:become(cd {} && yazi)\'
--bind \'ctrl-i:execute-silent(idea {} &)+abort\'
--bind \'ctrl-g:execute(cd {} && tv ctext)\'
--preview-window \'right,40%\'
--preview \'eza --tree --icons --level=1 --color=always --group-directories-first {}\''


alias grepid="grepf idea";
alias grepco="grepf code";
alias grepvi="grepf nvim";
alias grepsu="grepf subl";

# --preview-window \'right,50%,border-bottom,+{2}+3/2,~3\'

# Press F1 to open the file with less without leaving fzf
# Press CTRL-Y to copy the line to clipboard and aborts fzf (requires pbcopy)
# fzf --bind 'f1:execute(less -f {}),ctrl-y:execute-silent(echo {} | pbcopy)+abort'

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
# fd [OPTIONS] [pattern] [path].
alias grepid="grepf idea";
alias grepco="grepf code";
alias grepvi="grepf nvim";
alias grepsu="grepf subl";

function fzf_preview() {
    while IFS= read -r line; do
        echo "$line"
    done | fzf --ansi \
        --exact \
        --delimiter : \
        --preview 'bat --style=changes --color=always {1} --highlight-line {2}' \
        --preview-window 'right,50%,+{2}+3/2,~3' \
        --bind "enter:become(LIMITED=Y nvim {1})"
    # --bind "enter:become(bat --color=always {1} --highlight-line {2} --pager=\"less +{2}G -j 10\")"
    #--preview-window 'up,85%,border-bottom,+{2}+3/2,~3' \
    #--color "hl:-1:underline,hl+:-1:underline:reverse" \
}

function fzf_preview_no_select() {
    while IFS= read -r line; do
        echo "$line"
    done | fzf --ansi \
        --exact \
        --delimiter : \
        --preview 'bat --style=changes --color=always {1}' \
        --bind "enter:become(bat --color=always {1})"

    # --preview-window 'up,85%,border-bottom' \
    # --color "hl:-1:underline,hl+:-1:underline:reverse" \
}

function findf() {
    if [ -z "$1" ]; then
        fd --type f --color=always --hidden --exclude .git | fzf_preview_no_select
    else
        search="$1"
        fd --type f --color=always --hidden --exclude .git "$search" | fzf_preview_no_select
    fi
}

function findf_src() {
    if [ -z "$1" ]; then
        fd --type f --color=always --hidden --exclude .git --exclude test | fzf_preview_no_select
    else
        search="$1"
        fd --type f --color=always --hidden --exclude .git  --exclude test "$search" | fzf_preview_no_select
    fi
}

function grept() {
    if [ -z "$1" ]; then
        rg -g '!node_modules*' -g '!target*' -g '!bin*' --color=always --line-number --no-heading --smart-case --fixed-strings "" "$PWD" | fzf_preview
    else
        search="$1"
        rg -g '!node_modules*' -g '!target*' -g '!bin*' --color=always --line-number --no-heading --smart-case "$search" "$PWD" | fzf_preview
    fi
}

function grept_in() {
    search="$1"
    search_in="$2"

    # --hidden
    rg -g '!node_modules*' -g "$search_in" -g '!target*' -g '!bin*' --color=always --line-number --no-heading --smart-case "$search" "$PWD" | fzf_preview
}

function grept_src() {
    search="$1"

    rg -g '!node_modules*' -g '!target*' -g '!test*' -g '!bin*' --color=always --line-number --no-heading --smart-case "$search" "$PWD" | fzf_preview
}

function grept_src_in() {
    search="$1"
    search_in="$2"

    rg -g '!node_modules*' -g "$search_in" -g '!target*' -g '!test*' -g '!bin*' --color=always --line-number --no-heading --smart-case "$search" "$PWD" | fzf_preview
}

# legacy

function findt() {
    rg -i -N -C 15 -g '!node_modules*' -g '!target*' -g '!bin*' "$1" .
}

function findtc() {
    rg -I -i -N -c -g '!node_modules*' -g '!target*' -g '!bin*' "$1" . | awk '{sum += $1} END {print sum}'
}

function findt_in() {
    rg -i -N -C 15 -g "$2" -g '!node_modules*' -g '!target*' -g '!bin*' "$1" .
}

function findt_in_r() {
    rg -l -i -N -g "$2" -g '!node_modules*' -g '!target*' -g '!bin*' "$1" . | xargs -n 1 sed -i "s/$1/$3/g"
}

function wallpapers() {
    cd ~/wallpapers/ && fzf --preview 'fzf-preview.sh {}'
}

# ============================
# =================== testing

# fkill - kill process
killp() {
    # with ability to updating the list of processes by pressing CTRL-R

    (date; command ps -ef) |
    fzf --exact --bind='ctrl-r:reload(date; ps -ef)' \
        --header=$'Press CTRL-R to reload\n\n' --header-lines=2 \
        --preview='echo {}' --preview-window=down,5,wrap | awk '{print $2}' | xargs kill -9
}

# ffind() {
#     fd --type file |
#       fzf --exact --prompt 'Files> ' \
    #           --header 'CTRL-T: Switch between Files/Directories' \
    #           --bind 'ctrl-t:transform:[[ ! $FZF_PROMPT =~ Files ]] &&
#                   echo "change-prompt(Files> )+reload(fd --type file)" ||
#                   echo "change-prompt(Directories> )+reload(fd --type directory)"' \
    #           --preview '[[ $FZF_PROMPT =~ Files ]] && bat --color=always {} || tree -C -L 1 {}'
# }

# ==============================================
# ======================= find and edit by EDITOR

# function fze() {
#     result=$(fzf --exact --ansi --info=inline --height 80% --layout reverse --border --preview='bat --color=always --style=numbers --line-range=:500 {}')
#     if [ -n "$result" ]; then
#         $EDITOR $result
#     fi
# }
#
# function fzc() {
#      result=$( ( \
    #         fd . --type f --color=always --hidden --exclude .git \
    #             $HOME/ \
    #             $HOME/.local/bin \
    #              --max-depth 1 \
    #         ; \
    #         fd . --type f --color=always --hidden --exclude .git \
    #             $HOME/.config/ranger \
    #             $HOME/.config/rofi \
    #             $HOME/.config/qtile \
    #             $HOME/.config/nvim \
    #             $HOME/.config/nitrogen \
    #             $HOME/.config/awesome \
    #             $HOME/.config/alacritty \
    #             /etc/keyd \
    #              --max-depth 2 \
    #         ; \
    #         fd . --type d --color=always --hidden --exclude .git $HOME/serhii.home/work/git.work \
    #             --max-depth 1 ) | ( fzf --exact --ansi --info=inline --height 80% --layout reverse --border --preview='bat --color=always --style=numbers --line-range=:500 {}' ) )
#
#         if [ -n "$result" ]; then
#             $EDITOR $result
#         fi
# }