# docs: https://docs.junit.org/6.1.0-M1/user-guide/#running-tests-console-launcher
#
# to disable extra info: --disable-ansi-colors --disable-banner
#
# ####################################################
# ############ 1. List of supported engines ##########
java -jar "$HOME"/tools/java-extensions/junit/junit-platform-console-standalone.jar engines --disable-banner
# examples:
# junit-jupiter (org.junit.jupiter:junit-jupiter-engine:6.0.1)
# junit-platform-suite (org.junit.platform:junit-platform-suite-engine:6.0.1)
# junit-vintage (org.junit.vintage:junit-vintage-engine:6.0.1)

# in case project has more that one engine for test, we can use `--include-engine junit-jupiter` to separate, or to not discover other, in case only one engine is using

# ####################################################
# ############### 2. Test discovery ##################

java -jar "$HOME"/tools/java-extensions/junit/junit-platform-console-standalone.jar discover \
    --include-classname="^(Test.*|.+[.$]Test.*|.*Tests?|I[Tt].*|.+[.$]I[Tt].*|.*I[Tt]?)$" \
    --class-path "$("$HOME"/dotfiles/work/java/mvn_cp_cache.sh "$PWD" test)" \
    --scan-class-path --include-engine junit-jupiter --disable-banner

java -jar "$HOME"/tools/java-extensions/junit/junit-platform-console-standalone.jar discover \
    --include-classname="^(Test.*|.+[.$]Test.*|.*Tests?|I[Tt].*|.+[.$]I[Tt].*|.*I[Tt]?)$" \
    --class-path "$("$HOME"/dotfiles/work/java/mvn_cp_cache.sh "$PWD" test)" \
    --select-method "ua.raiffeisen.payments.cardtransferinitiation.adapter.api.http.CardTransferInitiationIT#shouldRetrieveInvalidDebtorWhenAccountStatusIsClosed(java.lang.Object,java.lang.String,java.lang.String,java.lang.String)" \
    --include-engine junit-jupiter --disable-banner

# ####################################################
# ################ 3. Test execute ###################

# #########################################################################
# ##################### run all tests #####################

# 1. scan all tests
java -javaagent:"$HOME"/tools/java-extensions/jmockit/jmockit.jar \
    -jar "$HOME"/tools/java-extensions/junit/junit-platform-console-standalone.jar execute \
    --class-path "$("$HOME"/dotfiles/work/java/mvn_cp_cache.sh "$PWD" test)" \
    --include-classname="^(Test.*|.+[.$]Test.*|.*Tests?|I[Tt].*|.+[.$]I[Tt].*|.*I[Tt]?)$" \
    --include-engine junit-jupiter --disable-banner \
    --scan-classpath

# 2. run all tests in package (root in gereral to run all tests)
java -javaagent:"$HOME"/tools/java-extensions/jmockit/jmockit.jar \
    -jar "$HOME"/tools/java-extensions/junit/junit-platform-console-standalone.jar execute \
    --class-path "$("$HOME"/dotfiles/work/java/mvn_cp_cache.sh "$PWD" test)" \
    --include-classname="^(Test.*|.+[.$]Test.*|.*Tests?|I[Tt].*|.+[.$]I[Tt].*|.*I[Tt]?)$" \
    --include-engine junit-jupiter --disable-banner \
    --select-package ua.raiffeisen.payments.cardtransferinitiation

#--select-package ua.raiffeisen.payments.cardtransferinitiation.adapter.api.http

# #########################################################################
# ##################### run all class tests #####################

java -javaagent:"$HOME"/tools/java-extensions/jmockit/jmockit.jar \
    -jar "$HOME"/tools/java-extensions/junit/junit-platform-console-standalone.jar execute \
    --class-path "$("$HOME"/dotfiles/work/java/mvn_cp_cache.sh "$PWD" test)" \
    --select-class ua.raiffeisen.payments.cardtransferinitiation.adapter.api.http.CardTransferInitiationIT \
    --include-engine junit-jupiter --disable-banner

# --select-class ua.raiffeisen.payments.cardtransferinitiation.adapter.api.http.CardTransferInitiationIT \
    # #########################################################################
# ##################### run method without params #####################

java -javaagent:"$HOME"/tools/java-extensions/jmockit/jmockit.jar \
    -jar "$HOME"/tools/java-extensions/junit/junit-platform-console-standalone.jar execute \
    --class-path "$("$HOME"/dotfiles/work/java/mvn_cp_cache.sh "$PWD" test)" \
    --select-method "ua.raiffeisen.payments.cardtransferinitiation.adapter.api.http.CardTransferInitiationIT#shouldRetrieveBadRequestErrorWhenInvalidUetrWasUsed" \
    --include-engine junit-jupiter --disable-banner

# with debug
java -agentlib:jdwp=transport=dt_socket,server=y,suspend=y,address=5005 \
    -javaagent:"$HOME"/tools/java-extensions/jmockit/jmockit.jar \
    -jar "$HOME"/tools/java-extensions/junit/junit-platform-console-standalone.jar execute \
    --class-path "$("$HOME"/dotfiles/work/java/mvn_cp_cache.sh "$PWD" test)" \
    --select-method "ua.raiffeisen.payments.cardtransferinitiation.core.usecase.initiation.helper.OperationalCodeEvaluatorTest#shouldReceiveErrorWhenWrongDebtorBalanceGroup" \
    --include-engine junit-jupiter --disable-banner


# run parametrized test all methods
java -javaagent:"$HOME"/tools/java-extensions/jmockit/jmockit.jar \
    -jar "$HOME"/tools/java-extensions/junit/junit-platform-console-standalone.jar execute \
    --class-path "$("$HOME"/dotfiles/work/java/mvn_cp_cache.sh "$PWD" test)" \
    --select-method "ua.raiffeisen.payments.cardtransferinitiation.adapter.api.http.CardTransferInitiationIT#shouldRetrieveInvalidDebtorWhenAccountStatusIsClosed(ua.raiffeisen.payments.cardtransferinitiation.test.model.TestTransferDirection,java.lang.String,java.lang.String,java.lang.String)" \
    --include-engine junit-jupiter --disable-banner

java -javaagent:"$HOME"/tools/java-extensions/jmockit/jmockit.jar \
    -jar "$HOME"/tools/java-extensions/junit/junit-platform-console-standalone.jar execute \
    --class-path "$("$HOME"/dotfiles/work/java/mvn_cp_cache.sh "$PWD" test)" \
    --select-method "ua.raiffeisen.payments.cardtransferinitiation.adapter.api.http.CardTransferInitiationIT#shouldSuccessfullyHandleInitiationRequestWhenValidRequest(ua.raiffeisen.payments.cardtransferinitiation.adapter.api.http.CardTransferInitiationIT\$TestInitiationParams)" \
    --include-engine junit-jupiter --disable-banner

# run parametrized test first method
java -javaagent:"$HOME"/tools/java-extensions/jmockit/jmockit.jar \
    -jar "$HOME"/tools/java-extensions/junit/junit-platform-console-standalone.jar execute \
    --class-path "$("$HOME"/dotfiles/work/java/mvn_cp_cache.sh "$PWD" test)" \
    --select-iteration "method:ua.raiffeisen.payments.cardtransferinitiation.adapter.api.http.CardTransferInitiationIT#shouldSuccessfullyHandleInitiationRequestWhenValidRequest(ua.raiffeisen.payments.cardtransferinitiation.adapter.api.http.CardTransferInitiationIT\$TestInitiationParams)[0]" \
    --include-engine junit-jupiter --disable-banner

# run parametrized test range methods (from second to third)
java -javaagent:"$HOME"/tools/java-extensions/jmockit/jmockit.jar \
    -jar "$HOME"/tools/java-extensions/junit/junit-platform-console-standalone.jar execute \
    --class-path "$("$HOME"/dotfiles/work/java/mvn_cp_cache.sh "$PWD" test)" \
    --select-iteration "method:ua.raiffeisen.payments.cardtransferinitiation.adapter.api.http.CardTransferInitiationIT#shouldSuccessfullyHandleInitiationRequestWhenValidRequest(ua.raiffeisen.payments.cardtransferinitiation.adapter.api.http.CardTransferInitiationIT\$TestInitiationParams)[1,2]" \
    --include-engine junit-jupiter --disable-banner
