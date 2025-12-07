local M = {}

local maven_util = require("utils.java.maven-util")
local util = require("utils.common-util")
-- local util = require("utils.common-util")
local home = os.getenv("HOME")

-- sdk list java
-- sdk install java 25xxx-amzn
-- sdk list maven
-- sdk install maven 3.9.xxx
local java_dir = vim.fn.glob(home .. "/.sdkman/candidates/java/current")
local java_bin = java_dir .. "/bin/java"
--local java_google_style_file = home .. "/dotfiles/work/formatter/intellij-java-google-style.xml"
--local java_google_style_file = home .. "/dotfiles/work/formatter/default_intellij_eclipse.xml"

-- https://github.com/google/styleguide/blob/gh-pages/eclipse-java-google-style.xml
local java_google_style_file = home .. "/dotfiles/work/formatter/eclipse-java-google-style.xml"

M.java_dir = java_dir
M.java_bin = java_bin
M.java_google_style_file = java_google_style_file

-- local trace_class_pattern = "(.-)([^/]-)%.([^%.]+)%((.-):(%d+)%)"
-- cp_path, method, file, line in string.gmatch(trace, "([%w%.%/%_-]*)%.([%w_-]+)%(([%w%.%/%_-]+%.java):(%d+)") do
local java_mvn_class_pattern = "at%s+([^%s]-)([^/^%s]-)%.([^%.]+)%((.-)%.java:(%d+)%)"

-- local java_class_pattern = "%[([A-Z]+)%]%s+([^:]+)%:%[(%d+),(%d+)%]%s*([^\n]*)" -- for gmatch
local java_compile_java_pattern = "%[([A-Z]+)%]%s+([^:]+):%[(%d+),(%d+)%]%s*(.*)"

---@return { class_path: string,
---class_path_root: string,
---class_line_number: integer,
---line_start_position: integer,
---line_end_position: integer,
---method: string} | nil
M.parse_java_mvn_run_class_line = function(line)
    -- line = util.strip_ansi(line)
    local prefix, class_path, method, file_name, line_num = line:match(java_mvn_class_pattern)
    if class_path then
        return {
            class_path = class_path,
            class_path_root = vim.split(class_path, "%$")[1],
            class_line_number = (tonumber(line_num) or 1),
            line_start_position = string.find(line, prefix .. class_path) - 1,
            line_end_position = #line,
            method = method,
        }
    end
    return nil
end

-- print(M.parse_java_mvn_run_class_line([[
--         at org.springframework.validation.beanvalidation.MethodValidationInterceptor.invoke(MethodValidationInterceptor.java:168)
--         at org.springframework.aop.framework.ReflectiveMethodInvocation.proceed(ReflectiveMethodInvocation.java:184)
--         at org.springframework.aop.framework.CglibAopProxy$DynamicAdvisedInterceptor.intercept(CglibAopProxy.java:728)
--         at ua.raiffeisen.payments.cardtransferinitiation.adapter.api.http.controller.CardTransferController$$SpringCGLIB$$0.initiateCardTransfer(<generated>)
--         at java.base/jdk.internal.reflect.DirectMethodHandleAccessor.invoke(DirectMethodHandleAccessor.java:104)
--         at java.base/java.lang.reflect.Method.invoke(Method.java:565)
--         at org.springframework.web.reactive.result.method.InvocableHandlerMethod.lambda$invoke$0(InvocableHandlerMethod.java:208)
--         at reactor.core.publisher.MonoFlatMap$FlatMapMain.onNext(MonoFlatMap.java:132)
--         at reactor.core.publisher.MonoZip$ZipCoordinator.signal(MonoZip.java:297)
--         at reactor.core.publisher.MonoZip$ZipInner.onNext(MonoZip.java:478)
--         at reactor.core.publisher.MonoPeekTerminal$MonoTerminalPeekSubscriber.onNext(MonoPeekTerminal.java:180)
--         at reactor.core.publisher.FluxDefaultIfEmpty$DefaultIfEmptySubscriber.onNext(FluxDefaultIfEmpty.java:122)
--         at reactor.core.publisher.FluxSwitchIfEmpty$SwitchIfEmptySubscriber.onNext(FluxSwitchIfEmpty.java:74)
--         at reactor.core.publisher.FluxOnErrorResume$ResumeSubscriber.onNext(FluxOnErrorResume.java:79)
--         at reactor.core.publisher.FluxMapFuseable$MapFuseableSubscriber.onNext(FluxMapFuseable.java:129)
--         at reactor.core.publisher.FluxContextWrite$ContextWriteSubscriber.onNext(FluxContextWrite.java:107)
--         at reactor.core.publisher.FluxMapFuseable$MapFuseableConditionalSubscriber.onNext(FluxMapFuseable.java:299)
--         at reactor.core.publisher.FluxFilterFuseable$FilterFuseableConditionalSubscriber.onNext(FluxFilterFuseable.java:337)
--         at reactor.core.publisher.Operators$BaseFluxToMonoOperator.completePossiblyEmpty(Operators.java:2096)
--         at reactor.core.publisher.MonoCollect$CollectSubscriber.onComplete(MonoCollect.java:145)
--         at reactor.core.publisher.FluxContextWriteRestoringThreadLocals$ContextWriteRestoringThreadLocalsSubscriber.onComplete(FluxContextWriteRestoringThreadLocals.java:149)
--         at org.springframework.http.server.reactive.AbstractListenerReadPublisher$State.onAllDataRead(AbstractListenerReadPublisher.java:501)
--         at org.springframework.http.server.reactive.AbstractListenerReadPublisher.onAllDataRead(AbstractListenerReadPublisher.java:137)
--         at org.springframework.http.server.reactive.ServletServerHttpRequest$RequestBodyPublisher$RequestBodyPublisherReadListener.onAllDataRead(ServletServerHttpRequest.java:382)
--         at org.eclipse.jetty.ee10.servlet.HttpInput.readCallback(HttpInput.java:370)
--         at org.eclipse.jetty.server.handler.ContextHandler$ScopedContext.run(ContextHandler.java:1549)
--         at org.eclipse.jetty.server.handler.ContextHandler$ScopedContext.run(ContextHandler.java:1542)
--         at org.eclipse.jetty.ee10.servlet.ServletChannel.handle(ServletChannel.java:545)
--         at org.eclipse.jetty.server.handler.ContextHandler$ScopedContext.run(ContextHandler.java:1555)
--         at org.eclipse.jetty.server.handler.ContextHandler$ScopedContext.lambda$execute$0(ContextHandler.java:1572)
--         at org.eclipse.jetty.util.thread.QueuedThreadPool.runJob(QueuedThreadPool.java:981)
--         at org.eclipse.jetty.util.thread.QueuedThreadPool$Runner.doRunJob(QueuedThreadPool.java:1211)
--         at org.eclipse.jetty.util.thread.QueuedThreadPool$Runner.run(QueuedThreadPool.java:1166)
--         at java.base/java.lang.Thread.run(Thread.java:1474)
--         at ua.raiffeisen.payments.infra.test.core.util.AssertUtil.assertEquals(AssertUtil.java:35)
--         at ua.raiffeisen.payments.cardtransferinitiation.adapter.api.http.CardTransferInitiationIT.shouldRetrieveBadRequestErrorWhenHeaderRequestIdIsInvalid(CardTransferInitiationIT.java:317)
-- ]]))

---@return { file: string,
---lnum: integer,
---col: integer,
---end_col: integer,
---message: string,
---severity: string} | nil
M.parse_mvn_compile_java_line = function(line)
    -- line = util.strip_ansi(line)
    local level, file, lnum, col, msg = line:match(java_compile_java_pattern)
    if file then
        col = (tonumber(col) or 1) - 1
        return {
            file = file,
            lnum = (tonumber(lnum) or 1) - 1,
            col = col,
            end_col = col + 20,
            message = msg,
            -- severity = vim.diagnostic.severity.ERROR,
            severity = maven_util.to_severity(level),
        }
    end
    return nil
end

---@return [{ class_path: string,
---class_path_root: string,
---class_line_number: integer,
---line_start_position: integer,
---line_end_position: integer,
---method: string}]
M.parse_java_mvn_run_class_text = function(text)
    local items = {}
    for line in text:gmatch("[^\n]+") do
        local p = M.parse_java_mvn_run_class_line(line)
        if p then
            items[#items + 1] = p
        end
    end
    return items
end

---@return [{ file: string,
---lnum: integer,
---col: integer,
---end_col: integer,
---message: string,
---severity: string}]
M.parse_mvn_compile_java_text = function(text)
    local items = {}
    -- for line in trace:gmatch("[^\r\n]+") do
    for line in text:gmatch("[^\n]+") do
        local p = M.parse_mvn_compile_java_line(line)
        if p then
            items[#items + 1] = p
        end
    end
    return items
end

-- local root = vim.fn.getcwd()
-- local root = vim.fs.root(0, { ".git", "pom.xml", "mvnw", "gradlew", "build.gradle", "build.gradle.kts" })
-- local src_dir = root .. "/src/main/java/"
-- local test_dir = root .. "/src/test/java/"

M.java_class_to_proj_path = function(classname)
    local relative_path = classname:gsub("%.", "/") .. ".java"
    local file_path = vim.fn.glob("*/**/" .. relative_path)

    -- local file_path = vim.fn.findfile(file)
    -- resolve file full path from root

    if file_path ~= nil and #file_path ~= 0 then
        return file_path
    end

    return nil

    --[[ local full_path_src = src_dir .. relative_path
    local full_path_test = test_dir .. relative_path
    if util.is_file_exists(full_path_src) then
        return full_path_src
    elseif util.is_file_exists(full_path_test) then
        return full_path_test
    end
    -- then it's dependency lib
    return nil ]]
end

local java_root_files = {
    "pom.xml",
    "build.gradle",
    "build.gradle.kts",
    "settings.gradle",
    "settings.gradle.kts",
}

M.is_java_project = function()
    local root = vim.fn.getcwd()
    for _, f in ipairs(java_root_files) do
        if util.is_file_exists(root .. "/" .. f) then
            return true
        end
    end
    return false
end

return M
