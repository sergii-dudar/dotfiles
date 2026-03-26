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

function findd() {
    local layout
    local result_dir
    layout=$(detect_layout)
    if [ -z "$1" ]; then
        result_dir=$(tv cdirs --layout "$layout")
    else
        search="'$1"
        result_dir=$(tv cdirs --layout "$layout" -i "$search")
    fi
    # echo "$result_dir"
    # [[ -n "$result_dir" ]] && z "$result_dir"
    # bash -c "$cmd" >/dev/null 2>&1 &
    eval "$result_dir"
}

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