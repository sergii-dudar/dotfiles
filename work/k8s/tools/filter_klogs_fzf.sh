#!/usr/bin/env bash

# cat ./klogs/payment-charge-calculation-*/*.log \
    #     | rg --color never "(4293246e-e5d2-4580-ac99-dfe95428d0ae|3cd00798-e7bf-484f-90df-e23651651b00)" \
    #     | jq . | nviml -R -c 'setfiletype json' -

# bat -pp ./klogs/payment-charge-calculation-*/*.log \
    #     | rg --color never "(4293246e-e5d2-4580-ac99-dfe95428d0ae|3cd00798-e7bf-484f-90df-e23651651b00)" \
    #     | jq . | nviml -R -c 'setfiletype json' -

# rg --color never --no-filename "(4293246e-e5d2-4580-ac99-dfe95428d0ae|3cd00798-e7bf-484f-90df-e23651651b00)" ./klogs/**/*.log \
    #     | jq . \
    #     | LIMITED=Y nvim -R -c 'setfiletype json' -

# --header-first \
    # --preview 'bat -l json --wrap=auto --style=changes --color=always {1} --highlight-line {2}' \
    # --preview-window 'right,60%,+{2}/3'
#
# --bind 'ctrl-e:execute(LIMITED=Y nvim {1} +{2})' \
    # --bind 'enter:execute(LIMITED=Y nvim {1} +{2})'


# pattern=$1
# tmp=./klogs/tmp-$(date +%s).json
# rg --color never --no-filename "$pattern" ./klogs/**/*.log \
    #     | jq -s . > "$tmp" && LIMITED=Y nvim -R "$tmp"; rm "$tmp"

tmp=./klogs/tmp-$(date +%s).json
RG_PREFIX="rg --no-filename --no-line-number --no-heading --color=always --smart-case {q} ./klogs/**/*.log"
RG_PREFIX_NOCOL="rg --no-filename --no-line-number --no-heading --color=never --smart-case {q} ./klogs/**/*.log"
INITIAL_QUERY="${1:-}"
REFRESH="${2:-3}"  # auto-reload interval in seconds, 0 to disable

PORT=$((10000 + RANDOM % 50000))
RELOAD_PID=""

if [[ "$REFRESH" -gt 0 ]] 2>/dev/null; then
    (last_mtime=""; while sleep "$REFRESH"; do
            cur_mtime=$(stat -f %m ./klogs/**/*.log 2>/dev/null | sort -rn | head -1)
            [[ "$cur_mtime" == "$last_mtime" ]] && continue
            last_mtime="$cur_mtime"
            curl -s -XPOST "localhost:$PORT" -d "reload($RG_PREFIX)+last" 2>/dev/null || break
    done) &
    RELOAD_PID=$!
fi

sep="\033[0;35m\033[0m"
function _klabel() {
    echo -e "[\033[0;32m\033[1m$1\033[0m]:"
}
function _klabels() {
    echo -e " ${sep} [\033[0;32m\033[1m$1\033[0m]:"
}

invim=$'\033[38;5;32m\033[0m'
iselect=$'\033[38;5;135m\033[0m'
ihist=$'\033[38;5;32m\033[0m'


_fzf_header=" $(_klabel '󰘴r')Reload ${ihist} $(_klabels '󰘴a/󰘴d')Selecte/Deselect all ${iselect} $(_klabels '^q')Nvim and quit ${invim} $(_klabels 'enter')Nvim ${invim} "
selected=$(fzf --listen "$PORT" --ansi --disabled --multi --query "$INITIAL_QUERY" \
        --layout=reverse \
        --header "$_fzf_header" \
        --bind "start:reload($RG_PREFIX)+last" \
        --bind "change:reload(sleep 0.1; $RG_PREFIX || true)+last" \
        --bind "ctrl-r:reload($RG_PREFIX)+last" \
        --bind "enter:execute($RG_PREFIX_NOCOL | gsed 's/^[^{]*//' | jq -s . > $tmp && LIMITED=Y nvim -R -c 'setfiletype json' $tmp; rm -f $tmp)" \
        --bind 'ctrl-q:select-all+accept' \
        --bind 'ctrl-a:select-all,ctrl-d:deselect-all' \
        --color 'hl:-1:underline,hl+:-1:underline:reverse' \
        --prompt 'Filter k8s logs ❯ ' \
        --wrap-sign '' \
        --delimiter : \
    --border-label '   Filter Logs ' ) || { [[ -n "$RELOAD_PID" ]] && kill "$RELOAD_PID" 2>/dev/null; exit 0; }
[[ -n "$RELOAD_PID" ]] && kill "$RELOAD_PID" 2>/dev/null

echo "$selected" | gsed 's/^[^{]*//' | jq -s . > "$tmp" \
    && LIMITED=Y nvim -R -c 'setfiletype json' "$tmp"
rm -f "$tmp"
