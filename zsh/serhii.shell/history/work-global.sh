# alias k9sa='k9s -n all'

#alias az_dev='az aks get-credentials --resource-group dev --name aks-dev'
#alias az_qa='az aks get-credentials --resource-group qa --name aks-qa'
#alias az_uat='az aks get-credentials --resource-group uat --name aks-uat'

##alias k9sd='k8s_dev ; kubectl get pods -n payments-dev'
##alias k9sp='k8s_prod ; kubectl get pods -n payments-prod'

# kubectl config use-context aws-avalaunch-dev
# kubectl config use-context aws-avalaunch-prod
alias k9sd='k8s_dev ; kubectl get pods -n payments-dev'
alias k9sp='k8s_prod ; kubectl get pods -n payments-prod'

alias msa='mvn spotless:apply'
alias helmdu='helm dependency update'
alias helmu='cd helm; helmdu'

alias gitreset_comit='git reset HEAD~'

alias mvnu='mvn clean compile -P autoInstallPackage -U'
alias mvncc='msa ; mvn clean compile'
alias mvncv='msa ; mvn clean verify'
alias mcc='mvn clean compile'
#alias nativeb='mvn clean -Dcyclonedx.skip=true -Djacoco.skip=true -Dmaven.test.skip=true -DskipTests=true -DskipNativeTests -DquickBuild -Pnative native:compile'
#alias springbi='mvn clean -Dcyclonedx.skip=true -Djacoco.skip=true -Dmaven.test.skip=true -DskipTests=true spring-boot:build-image'
#alias springbi_n='mvn clean -Dcyclonedx.skip=true -Djacoco.skip=true -Dmaven.test.skip=true -DskipTests=true -Pnative spring-boot:build-image'
# ./target/user-service -Dspring.profiles.active=work,default
# java -Dspring.profiles.active=work,default -jar ./target/user-service-local.jar
# export GRAALVM_BUILDTOOLS_MAX_PARALLEL_BUILDS=8
# export APP_CLIENT_SERVICE_AUTH_URL=http://traefik-internal.test/auth/system

alias dup='docker-compose up'
alias ddown='docker-compose down'

# docker
alias dcu='docker-compose up'
alias dcd='docker-compose down'
alias dps='docker ps'

# temporal (localhost:7233, UI http://localhost:8233.)
#alias temporal_start='temporal server start-dev'
#alias temporal_start='temporal server start-dev --db-filename temporal_dev_store.db'
