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

### Find File ###

function findf() {
    local layout
    layout=$(detect_layout)
    if [ -z "$1" ]; then
        tv cfiles --layout "$layout"
    else
        search="'$1"
        tv cfiles --layout "$layout" -i "$search"
    fi
}

function findf_src() {
    local layout
    local header
    local source_cmd
    layout=$(detect_layout)
    header="findt src"
    source_cmd="fd --type f --color=always --hidden --exclude .git --exclude test"
    if [ -z "$1" ]; then
        tv cfiles --layout "$layout" --input-header "$header" --source-command "$source_cmd"
    else
        search="'$1"
        tv cfiles --layout "$layout" --input-header "$header" -i "$search" --source-command "$source_cmd"
    fi
}

### Find Dir ###

# TODO: dind dirs, and open with nvim/idea/yazi/just cd

### Grep ###

function grept() {
    local layout
    layout=$(detect_layout)
    if [ -z "$1" ]; then
        tv ctext --layout "$layout"
    else
        search="'$1"
        tv ctext --layout "$layout" -i "$search"
    fi
}

function grept_in() {
    local layout
    local search_in
    local header
    local source_cmd
    layout=$(detect_layout)
    search_in="*.$2"
    header="grept in ( $search_in )"
    source_cmd="rg . -g '!node_modules*' -g '$search_in' -g '!target*' -g '!bin*' --color=always --line-number --no-heading --smart-case --fixed-strings"
    if [ -z "$1" ]; then
        tv ctext --layout "$layout" --input-header "$header" --source-command "$source_cmd"
    else
        search="'$1"
        tv ctext --layout "$layout" --input-header "$header" -i "$search" --source-command "$source_cmd"
    fi
}

function grept_src() {
    local layout
    local header
    local source_cmd
    layout=$(detect_layout)
    header="grept src"
    source_cmd="rg . -g '!node_modules*' -g '!target*' -g '!test*' -g '!bin*' --color=always --line-number --no-heading --smart-case --fixed-strings"
    if [ -z "$1" ]; then
        tv ctext --layout "$layout" --input-header "$header" --source-command "$source_cmd"
    else
        search="'$1"
        tv ctext --layout "$layout" --input-header "$header" -i "$search" --source-command "$source_cmd"
    fi
}

function grept_src_in() {
    local layout
    local search_in
    local header
    local source_cmd
    layout=$(detect_layout)
    search_in="*.$2"
    header="grept src in ( $search_in )"
    source_cmd="rg . -g '!node_modules*' -g '$search_in' -g '!target*' -g '!test*' -g '!bin*' --color=always --line-number --no-heading --smart-case --fixed-strings"
    if [ -z "$1" ]; then
        tv ctext --layout "$layout" --input-header "$header" --source-command "$source_cmd"
    else
        search="'$1"
        tv ctext --layout "$layout" --input-header "$header" -i "$search" --source-command "$source_cmd"
    fi
}