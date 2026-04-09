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

function _klabel() {
    echo "[${GREEN}${BOLD}$1$RESET]:"
}
function _klabels() {
    echo " ${sep} [${GREEN}${BOLD}$1$RESET]:"
}

ifiles=$'\033[38;5;63m\033[0m'
ifiles_src=$'\033[38;5;4m\033[0m'

itmux=$'\033[38;5;4m\033[0m'
ifolder=$'\033[38;5;130m\033[0m'
invim=$'\033[38;5;32m\033[0m'
isubl=$'\033[38;5;32m\033[0m'
icopy=$'\033[38;5;135m\033[0m'
ihist=$'\033[38;5;32m\033[0m'
izoxide=$'\033[38;5;14m󱓞\033[0m'
isearch=$'\033[38;5;141m\033[0m'
iterm=$'\033[38;5;4m\033[0m'

iidea=$'\033[38;5;12m\033[0m'
ienter=$'\033[38;5;69m󰿄\033[0m'

iapply=$'\033[38;5;32m\033[0m'

sep="${MAGENTA}$RESET"

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
--pointer '󰁕'
--preview-border=none
--color=$fzf_colors,header:italic
--ansi --height 100% --layout reverse
--border --style minimal
--info=inline-right
--highlight-line --cycle --wrap-word
--prompt='❯ ' --no-separator
--bind 'ctrl-h:toggle-preview'
--bind 'ctrl-/:change-preview-window(down|)'"

# ===== CTRL-T runs $FZF_CTRL_T_COMMAND to get a list of files and directories
# --no-ignore
_clip=$(command -v pbcopy || command -v wl-copy || echo "xclip -selection clipboard")
export FZF_CTRL_T_COMMAND='fd --type file --color=always --hidden --exclude .git --exclude node_modules'
_fzf_crtl_t_header=" $(_klabel '󰘴t')Switch ( Files ${ifiles} / Src ${ifiles_src} )$(_klabels '󰘴y')Yazi ${ifolder} $(_klabels 'enter')Nvim ${invim}$(_klabels '󰘴s')Subl ${isubl}$(_klabels '󰘴n')Term ${iterm}$(_klabels '󰘴c')Copy ${icopy} "
export FZF_CTRL_T_OPTS=$'
--exact
--border-label \'  Files Manager \'
--prompt \' Files ❯ \'
--header \''"${_fzf_crtl_t_header}"$'\'
--preview-window \'right,60%\'
--bind \'ctrl-t:transform:[[ ! $FZF_PROMPT =~ Src ]] &&
echo "change-prompt( Files Src ❯ )+reload(fd --type file --color=always --hidden --exclude .git --exclude node_modules --exclude test)" ||
echo "change-prompt( Files ❯ )+reload(fd --type file --color=always --hidden --exclude .git --exclude node_modules)"\'
--bind \'ctrl-s:execute(subl {} &)+abort\'
--bind \'ctrl-c:execute-silent(echo -n {} | \''"${_clip}"$'\')+abort\'
--bind \'ctrl-y:execute(cd $(dirname {}) && yazi)\'
--bind \'ctrl-n:execute-silent(alacritty --working-directory $PWD/$(dirname {}))\'
--bind \'enter:execute(LIMITED=Y nvim {})+abort\'
--preview \'bat --style=changes --color=always {}\''
# --walker-skip .git,node_modules,target,bin

# ===== CTRL-R - Paste the selected command from history onto the command-line
# CTRL-/ to toggle small preview window to see the full command
# CTRL-Y to copy the command into clipboard using pbcopy
_fzf_crtl_r_header=" $(_klabel '󰘴c')Copy ${icopy} $(_klabels 'enter')Apply ${iapply}"
export FZF_CTRL_R_OPTS=$'
--border-label \'   Commands History \'
--prompt \'  Cmd History ❯ \'
--header \''"${_fzf_crtl_r_header}"$'\'
--preview \'echo {}\'
--preview-window up:3:hidden:wrap
--bind \'ctrl-c:execute-silent(echo -n {2..} | \''"${_clip}"$'\')+abort\''

# ===== ALT-C runs $FZF_ALT_C_COMMAND to get a list of directories
# export FZF_ALT_C_COMMAND='fd --type d --color=always --hidden --exclude .git'
export FZF_ALT_C_COMMAND='zoxide query -l'
_fzf_alt_c_header=" $(_klabel '󰘴t')Switch (Z ${izoxide} / Dirs ${isearch} )$(_klabels '󰘴e')Nvim ${invim} $(_klabels '󰘴i')Idea ${iidea}$(_klabels '󰘴y')Yazi ${ifolder} $(_klabels '󰘴n')Term ${iterm}$(_klabels '󰘴c')Copy paht ${icopy}$(_klabels 'enter')CD ${ienter}"
export FZF_ALT_C_OPTS=$'
--exact
--prompt \'󱓞 Zoxide ❯ \'
--header \''"${_fzf_alt_c_header}"$'\'
--border-label \' Directories Manager \'
--bind \'ctrl-t:transform:[[ ! $FZF_PROMPT =~ Zoxide ]] &&
echo "change-prompt(󱓞 Zoxide ❯ )+reload(zoxide query -l)" ||
echo "change-prompt(  Dirs ❯ )+reload(fd . --type directory --hidden --exclude .git --exclude target --exclude bin $HOME)"\'
--bind \'ctrl-e:become(cd {} && nvim)\'
--bind \'ctrl-y:become(cd {} && yazi)\'
--bind \'ctrl-i:execute-silent(idea {} &)+abort\'
--bind \'ctrl-c:execute-silent(echo -n {} | \''"${_clip}"$'\')\'
--bind \'ctrl-n:execute-silent(alacritty --working-directory {})\'
--preview-window \'right,40%\'
--preview \'eza --tree --icons --level=1 --color=always --group-directories-first {}\''

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

function grept() {
    rm -f /tmp/rg-fzf-src
    RG_PREFIX="rg -g '!node_modules*' -g '!target*' -g '!bin*' --column --line-number --no-heading --color=always --smart-case "
    local prompt=" Search ❯ "
    local prompt_src=" Search Src ❯ "

    if [ -n "$2" ]; then
        local search_in="*.$2"
        RG_PREFIX="$RG_PREFIX -g '$search_in' "
        prompt=" Search ( $search_in ) ❯ "
        prompt_src=" Search Src ( $search_in ) ❯ "
    fi

    RG_SRC="$RG_PREFIX -g '!test*' "
    # INITIAL_QUERY="${*:-}"
    INITIAL_QUERY="${1:-}"
    _fzf_ctr_g_header=" $(_klabel '󰘴t')Switch ( Files ${ifiles} / Src ${ifiles_src} )$(_klabels '󰘴y')Yazi ${ifolder} $(_klabels '󰘴n')Term ${iterm}$(_klabels '^e')Nvim(Peek) ${invim} $(_klabels '󰘴c')Copy paht ${icopy} $(_klabels 'enter')Nvim ${invim} "
    fzf --ansi --disabled --multi --query "$INITIAL_QUERY" \
        --bind "start:reload:$RG_PREFIX {q}" \
        --bind "change:reload:sleep 0.1; if [ -f /tmp/rg-fzf-src ]; then $RG_SRC {q}; else $RG_PREFIX {q}; fi || true" \
        --bind 'ctrl-a:select-all,ctrl-d:deselect-all' \
        --bind "ctrl-t:transform:[[ ! \$FZF_PROMPT =~ Src ]] &&
    echo \"change-prompt($prompt_src)+execute-silent(touch /tmp/rg-fzf-src)+reload($RG_SRC {q})\" ||
    echo \"change-prompt($prompt)+execute-silent(rm -f /tmp/rg-fzf-src)+reload($RG_PREFIX {q})\"" \
        --color "hl:-1:underline,hl+:-1:underline:reverse" \
        --prompt "$prompt" \
        --delimiter : \
        --border-label '   Search Manager ' \
        --header-first \
        --header "$_fzf_ctr_g_header" \
        --preview 'bat --style=changes --color=always {1} --highlight-line {2}' \
        --preview-window 'right,60%,+{2}/3' \
        --bind "ctrl-y:execute(cd \$(dirname {1}) && yazi)" \
        --bind "ctrl-c:execute-silent(echo -n \${PWD}/{1} | ${_clip})" \
        --bind "ctrl-n:execute-silent(alacritty --working-directory \${PWD}/\$(dirname {1}))" \
        --bind 'ctrl-e:execute(LIMITED=Y nvim {1} +{2})' \
        --bind 'enter:execute(LIMITED=Y nvim {1} +{2})'
    #--bind 'enter:become(LIMITED=Y nvim {1} +{2})'

## example with ability to swith rg and fzf filtering
#     rm -f /tmp/rg-fzf-{r,f}
# RG_PREFIX="rg -g '!node_modules*' -g '!target*' -g '!bin*' --column --line-number --no-heading --color=always --smart-case "
# INITIAL_QUERY="${*:-}"
## --header 'CTRL-S: Switch between rg/fzf' \
# fzf --ansi --disabled --multi --query "$INITIAL_QUERY" \
#     --bind "start:reload:$RG_PREFIX {q}" \
#     --bind "change:reload:sleep 0.1; $RG_PREFIX {q} || true" \
#     --bind 'alt-a:select-all,alt-d:deselect-all' \
#     --bind "ctrl-s:transform:[[ ! \$FZF_PROMPT =~ rg ]] &&
# echo \"rebind(change)+change-prompt(rg ❯ )+disable-search+transform-query:echo \\{q} > /tmp/rg-fzf-f; cat /tmp/rg-fzf-r\" ||
# echo \"unbind(change)+change-prompt(fzf ❯ )+enable-search+transform-query:echo \\{q} > /tmp/rg-fzf-r; cat /tmp/rg-fzf-f\"" \
#     --color "hl:-1:underline,hl+:-1:underline:reverse" \
#     --prompt 'rg ❯ ' \
#     --delimiter : \
#     --border-label '   Search Manager ' \
#     --header-first \
#     --header "$_fzf_ctr_g_header" \
#     --preview 'bat --style=changes --color=always {1} --highlight-line {2}' \
#     --preview-window 'right,60%,+{2}/3' \
#     --bind 'enter:become(LIMITED=Y nvim {1} +{2})'
}

function _grept_widget() {
    emulate -L zsh
    zle -I
    [[ -n "$zle_bracketed_paste" ]] && print -nr "${zle_bracketed_paste[2]}" >"${TTY:-/dev/tty}"
    grept "$@" </dev/tty
    [[ -n "$zle_bracketed_paste" ]] && print -nr "${zle_bracketed_paste[1]}" >"${TTY:-/dev/tty}"
    zle reset-prompt
}
zle -N _grept_widget
bindkey '^g' _grept_widget

function grept_in() {
    grept "$1" "$2"
}

# ==================================================
# ================= searching file content \ replace

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