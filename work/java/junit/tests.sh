# docs: https://docs.junit.org/6.1.0-M1/user-guide/#running-tests-console-launcher
#
# to disable extra info: --disable-ansi-colors --disable-banner
#
# ####################################################
# ############ 1. List of supported engines ##########
java -jar "$HOME"/tools/java-extensions/junit/junit-platform-console-standalone-*.jar engines --disable-banner
# examples:
# junit-jupiter (org.junit.jupiter:junit-jupiter-engine:6.0.1)
# junit-platform-suite (org.junit.platform:junit-platform-suite-engine:6.0.1)
# junit-vintage (org.junit.vintage:junit-vintage-engine:6.0.1)

# in case project has more that one engine for test, we can use `--include-engine junit-jupiter` to separate, or to not discover other, in case only one engine is using

# ####################################################
# ############### 2. Test discovery ##################

java -jar "$HOME"/tools/java-extensions/junit/junit-platform-console-standalone-*.jar discover \
    --class-path "$("$HOME"/dotfiles/work/java/mvn_cp_cache.sh ./ test)" \
    --scan-class-path --include-engine junit-jupiter --disable-banner

# ####################################################
# ################ 3. Test execute ###################

# 3.1. scann all tests in cp and run
java -jar "$HOME"/tools/java-extensions/junit/junit-platform-console-standalone-*.jar execute \
    --class-path "$("$HOME"/dotfiles/work/java/mvn_cp_cache.sh ./ test)" \
    --scan-class-path --include-engine junit-jupiter --disable-banner

# 3.2. run all tests in package
java -jar "$HOME"/tools/java-extensions/junit/junit-platform-console-standalone-*.jar execute \
    --class-path "$("$HOME"/dotfiles/work/java/mvn_cp_cache.sh ./ test)" \
    --select-package ua.serhii.application \
    --include-engine junit-jupiter --disable-banner

# 3.3. run all tests in single class
java -jar "$HOME"/tools/java-extensions/junit/junit-platform-console-standalone-*.jar execute \
    --class-path "$("$HOME"/dotfiles/work/java/mvn_cp_cache.sh ./ test)" \
    --select-class ua.serhii.application.Tests1 \
    --include-engine junit-jupiter --disable-banner

# 3.4. run specific test method
java -jar "$HOME"/tools/java-extensions/junit/junit-platform-console-standalone-*.jar execute \
    --class-path "$("$HOME"/dotfiles/work/java/mvn_cp_cache.sh ./ test)" \
    --select-method ua.serhii.application.Tests2#testSomething3 \
    --include-engine junit-jupiter --disable-banner

# test reposrt:
# --reports-dir=./test-report