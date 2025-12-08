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
    --class-path "$("$HOME"/dotfiles/work/java/mvn_cp_cache.sh "$PWD" test)" \
    --scan-class-path --include-engine junit-jupiter --disable-banner

# ####################################################
# ################ 3. Test execute ###################

# #########################################################################
# ##################### run all tests #####################

# 1. scan all tests
java -javaagent:"$HOME"/.m2/repository/org/jmockit/jmockit/1.50/jmockit-1.50.jar \
    -jar "$HOME"/tools/java-extensions/junit/junit-platform-console-standalone-*.jar execute \
    --class-path "$("$HOME"/dotfiles/work/java/mvn_cp_cache.sh "$PWD" test)" \
    --include-classname="^(Test.*|.+[.$]Test.*|.*Tests?|I[Tt].*|.+[.$]I[Tt].*|.*I[Tt]?)$" \
    --include-engine junit-jupiter --disable-banner \
    --scan-classpath

# 2. run all tests in package (root in gereral to run all tests)
java -javaagent:"$HOME"/.m2/repository/org/jmockit/jmockit/1.50/jmockit-1.50.jar \
    -jar "$HOME"/tools/java-extensions/junit/junit-platform-console-standalone-*.jar execute \
    --class-path "$("$HOME"/dotfiles/work/java/mvn_cp_cache.sh "$PWD" test)" \
    --include-classname="^(Test.*|.+[.$]Test.*|.*Tests?|I[Tt].*|.+[.$]I[Tt].*|.*I[Tt]?)$" \
    --include-engine junit-jupiter --disable-banner \
    --select-package ua.raiffeisen.payments.cardtransferinitiation

#--select-package ua.raiffeisen.payments.cardtransferinitiation.adapter.api.http

# #########################################################################
# ##################### run all class tests #####################

java -javaagent:"$HOME"/.m2/repository/org/jmockit/jmockit/1.50/jmockit-1.50.jar \
    -jar "$HOME"/tools/java-extensions/junit/junit-platform-console-standalone-*.jar execute \
    --class-path "$("$HOME"/dotfiles/work/java/mvn_cp_cache.sh "$PWD" test)" \
    --select-class ua.raiffeisen.payments.cardtransferinitiation.adapter.api.http.CardTransferInitiationIT \
    --include-engine junit-jupiter --disable-banner

# #########################################################################
# ##################### run method without params #####################

java -javaagent:"$HOME"/.m2/repository/org/jmockit/jmockit/1.50/jmockit-1.50.jar \
    -jar "$HOME"/tools/java-extensions/junit/junit-platform-console-standalone-*.jar execute \
    --class-path "$("$HOME"/dotfiles/work/java/mvn_cp_cache.sh "$PWD" test)" \
    --select-method "ua.raiffeisen.payments.cardtransferinitiation.adapter.api.http.CardTransferInitiationIT#shouldRetrieveBadRequestErrorWhenInvalidUetrWasUsed" \
    --include-engine junit-jupiter --disable-banner

# run parametrized test all methods
java -javaagent:"$HOME"/.m2/repository/org/jmockit/jmockit/1.50/jmockit-1.50.jar \
    -jar "$HOME"/tools/java-extensions/junit/junit-platform-console-standalone-*.jar execute \
    --class-path "$("$HOME"/dotfiles/work/java/mvn_cp_cache.sh "$PWD" test)" \
    --select-method "ua.raiffeisen.payments.cardtransferinitiation.adapter.api.http.CardTransferInitiationIT#shouldRetrieveInvalidDebtorWhenAccountStatusIsClosed(ua.raiffeisen.payments.cardtransferinitiation.test.model.TestTransferDirection,java.lang.String,java.lang.String,java.lang.String)" \
    --include-engine junit-jupiter --disable-banner

java -javaagent:"$HOME"/.m2/repository/org/jmockit/jmockit/1.50/jmockit-1.50.jar \
    -jar "$HOME"/tools/java-extensions/junit/junit-platform-console-standalone-*.jar execute \
    --class-path "$("$HOME"/dotfiles/work/java/mvn_cp_cache.sh "$PWD" test)" \
    --select-method "ua.raiffeisen.payments.cardtransferinitiation.adapter.api.http.CardTransferInitiationIT#shouldSuccessfullyHandleInitiationRequestWhenValidRequest(ua.raiffeisen.payments.cardtransferinitiation.adapter.api.http.CardTransferInitiationIT\$TestInitiationParams)" \
    --include-engine junit-jupiter --disable-banner

# run parametrized test first method
java -javaagent:"$HOME"/.m2/repository/org/jmockit/jmockit/1.50/jmockit-1.50.jar \
    -jar "$HOME"/tools/java-extensions/junit/junit-platform-console-standalone-*.jar execute \
    --class-path "$("$HOME"/dotfiles/work/java/mvn_cp_cache.sh "$PWD" test)" \
    --select-iteration "method:ua.raiffeisen.payments.cardtransferinitiation.adapter.api.http.CardTransferInitiationIT#shouldSuccessfullyHandleInitiationRequestWhenValidRequest(ua.raiffeisen.payments.cardtransferinitiation.adapter.api.http.CardTransferInitiationIT\$TestInitiationParams)[0]" \
    --include-engine junit-jupiter --disable-banner

# run parametrized test range methods (from second to third)
java -javaagent:"$HOME"/.m2/repository/org/jmockit/jmockit/1.50/jmockit-1.50.jar \
    -jar "$HOME"/tools/java-extensions/junit/junit-platform-console-standalone-*.jar execute \
    --class-path "$("$HOME"/dotfiles/work/java/mvn_cp_cache.sh "$PWD" test)" \
    --select-iteration "method:ua.raiffeisen.payments.cardtransferinitiation.adapter.api.http.CardTransferInitiationIT#shouldSuccessfullyHandleInitiationRequestWhenValidRequest(ua.raiffeisen.payments.cardtransferinitiation.adapter.api.http.CardTransferInitiationIT\$TestInitiationParams)[1,2]" \
    --include-engine junit-jupiter --disable-banner

