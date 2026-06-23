curl -s "https://get.sdkman.io" | bash && \
export SDKMAN_DIR="$HOME/.sdkman" && \
[[ -s "$HOME/.sdkman/bin/sdkman-init.sh" ]] && source "$HOME/.sdkman/bin/sdkman-init.sh" && \
sdk install java 21.0.4-oracle && \
sdk install gradle && \
sdk install maven