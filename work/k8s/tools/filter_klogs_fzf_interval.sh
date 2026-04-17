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
INITIAL_QUERY="${1:-}"
selected=$(fzf --ansi --disabled --multi --query "$INITIAL_QUERY" \
        --bind "start:reload:$RG_PREFIX" \
        --bind "change:reload:sleep 0.1; $RG_PREFIX || true" \
        --bind "ctrl-r:reload:$RG_PREFIX" \
        --bind 'enter:select-all+accept' \
        --bind 'ctrl-a:select-all,ctrl-d:deselect-all' \
        --color 'hl:-1:underline,hl+:-1:underline:reverse' \
        --prompt 'Filter ❯ ' \
        --wrap-sign '' \
        --delimiter : \
    --border-label '   Filter Logs ' ) || exit 0

echo "$selected" | gsed 's/^[^{]*//' | jq -s . > "$tmp" \
    && LIMITED=Y nvim -R -c 'setfiletype json' "$tmp"
rm -f "$tmp"


