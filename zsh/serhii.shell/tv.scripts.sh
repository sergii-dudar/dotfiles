function fzf_preview() {
    while IFS= read -r line; do
        echo "$line"
    done | fzf --ansi \
        --exact \
        --color "hl:-1:underline,hl+:-1:underline:reverse" \
        --delimiter : \
        --preview 'bat --style=changes --color=always {1} --highlight-line {2}' \
        --preview-window 'up,85%,border-bottom,+{2}+3/2,~3' \
        --bind "enter:become(LIMITED=Y nvim {1})"
    # --bind "enter:become(bat --color=always {1} --highlight-line {2} --pager=\"less +{2}G -j 10\")"
}

function fzf_preview_no_select() {
    while IFS= read -r line; do
        echo "$line"
    done | fzf --ansi \
        --exact \
        --color "hl:-1:underline,hl+:-1:underline:reverse" \
        --delimiter : \
        --preview 'bat --style=changes --color=always {1}' \
        --preview-window 'up,85%,border-bottom' \
        --bind "enter:become(bat --color=always {1})"
}

function detect_layout() {
    local cols lines
    # if [[ -n "$TMUX" ]]; then
    #     cols=$(tmux display -p '#{pane_width}')
    #     lines=$(tmux display -p '#{pane_height}')
    # else
    cols=${COLUMNS:-$(tput cols)}
    lines=${LINES:-$(tput lines)}
    # echo $((cols / 3.2)) - $((lines))
    # echo "$(echo "scale=2; $cols / 3" | bc)" - $((lines))
    # echo $((cols / 3)) - $((lines))

    # fi

    # if (( cols > 2 * lines )); then
    # if (( cols * 2 > lines * 3 )); then
    # if (( lines > cols / 3 )); then
    if (( lines > "$(echo "scale=2; $cols / 3.2" | bc)" )); then
        echo "portrait"
    else
        echo "landscape"
    fi
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
    local layout
    layout=$(detect_layout)
    if [ -z "$1" ]; then
        tv ctext --layout "$layout"
        # rg -g '!node_modules*' -g '!target*' -g '!bin*' --color=always --line-number --no-heading --smart-case --fixed-strings "" "$PWD" | fzf_preview
    else
        search="$1"
        tv ctext --layout "$layout" -i "$search"
        # rg -g '!node_modules*' -g '!target*' -g '!bin*' --color=always --line-number --no-heading --smart-case "$search" "$PWD" | fzf_preview
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