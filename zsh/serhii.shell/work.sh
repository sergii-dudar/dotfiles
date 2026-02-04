alias k9sdev='k9s -n payments-dev --context aws-avalaunch-dev -c deployments'
alias k9suat='k9s -n payments-uat --context aws-avalaunch-dev -c deployments'
alias k9sprod='k9s -n payments-prod --context aws-avalaunch-prod -c deployments'
# alias k9sprod='k9s -n payments-prod --context aws-avalaunch-prod -c deployments --readonly'
# alias k9sprod_rw='k9s -n payments-prod --context aws-avalaunch-prod -c deployments'

alias k9sdev_p='k9s -n payments-dev --context aws-avalaunch-dev -c pods'
alias k9suat_p='k9s -n payments-uat --context aws-avalaunch-dev -c pods'
alias k9sprod_p='k9s -n payments-prod --context aws-avalaunch-prod -c pods'
# alias k9sprod_p='k9s -n payments-prod --context aws-avalaunch-prod -c pods --readonly'
# alias k9sprod_p_rw='k9s -n payments-prod --context aws-avalaunch-prod -c pods'

alias k9sdev_m='k9s -n payments-dev --context aws-avalaunch-dev -c configmaps'
alias k9suat_m='k9s -n payments-uat --context aws-avalaunch-dev -c configmaps'
alias k9sprod_m='k9s -n payments-prod --context aws-avalaunch-prod -c configmaps'
# alias k9sprod_m='k9s -n payments-prod --context aws-avalaunch-prod -c configmaps --readonly'
# alias k9sprod_m_rw='k9s -n payments-prod --context aws-avalaunch-prod -c configmaps'

alias k9sdatadev='k9s -n payments-data-dev --context aws-avalaunch-dev -c pods'
alias k9sdatauat='k9s -n payments-data-uat --context aws-avalaunch-dev -c pods'
alias k9sdataprod='k9s -n payments-data-prod --context aws-avalaunch-prod -c pods'

alias service_versions="list-service-versions.sh | fzf"

export PATH="$PATH:$HOME/serhii.home/work/git.tools/ua-payments-tools/tools/k8s"

# export PATH="$PATH:$HOME/serhii.home/work/git.tools/ua-payments-tools/tools/k8s"