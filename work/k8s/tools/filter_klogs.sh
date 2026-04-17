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


pattern=$1
tmp=./klogs/tmp-$(date +%s).json
rg --color never --no-filename "$pattern" ./klogs/**/*.log \
    | jq -s . > "$tmp" && LIMITED=Y nvim -R "$tmp"; rm "$tmp"
