local java_util = require("utils.java.java-common")

--[[
        at org.springframework.validation.beanvalidation.MethodValidationInterceptor.invoke(MethodValidationInterceptor.java:168)
        at org.springframework.aop.framework.ReflectiveMethodInvocation.proceed(ReflectiveMethodInvocation.java:184)
        at org.springframework.aop.framework.CglibAopProxy$DynamicAdvisedInterceptor.intercept(CglibAopProxy.java:728)
        at ua.raiffeisen.payments.cardtransferinitiation.adapter.api.http.controller.CardTransferController$$SpringCGLIB$$0.initiateCardTransfer(<generated>)
        at java.base/jdk.internal.reflect.DirectMethodHandleAccessor.invoke(DirectMethodHandleAccessor.java:104)
        at java.base/java.lang.reflect.Method.invoke(Method.java:565)
        at org.springframework.web.reactive.result.method.InvocableHandlerMethod.lambda$invoke$0(InvocableHandlerMethod.java:208)
        at reactor.core.publisher.MonoFlatMap$FlatMapMain.onNext(MonoFlatMap.java:132)
        at reactor.core.publisher.MonoZip$ZipCoordinator.signal(MonoZip.java:297)
        at reactor.core.publisher.MonoZip$ZipInner.onNext(MonoZip.java:478)
        at reactor.core.publisher.MonoPeekTerminal$MonoTerminalPeekSubscriber.onNext(MonoPeekTerminal.java:180)
        at reactor.core.publisher.FluxDefaultIfEmpty$DefaultIfEmptySubscriber.onNext(FluxDefaultIfEmpty.java:122)
        at reactor.core.publisher.FluxSwitchIfEmpty$SwitchIfEmptySubscriber.onNext(FluxSwitchIfEmpty.java:74)
        at reactor.core.publisher.FluxOnErrorResume$ResumeSubscriber.onNext(FluxOnErrorResume.java:79)
        at reactor.core.publisher.FluxMapFuseable$MapFuseableSubscriber.onNext(FluxMapFuseable.java:129)
        at reactor.core.publisher.FluxContextWrite$ContextWriteSubscriber.onNext(FluxContextWrite.java:107)
        at reactor.core.publisher.FluxMapFuseable$MapFuseableConditionalSubscriber.onNext(FluxMapFuseable.java:299)
        at reactor.core.publisher.FluxFilterFuseable$FilterFuseableConditionalSubscriber.onNext(FluxFilterFuseable.java:337)
        at reactor.core.publisher.Operators$BaseFluxToMonoOperator.completePossiblyEmpty(Operators.java:2096)
        at reactor.core.publisher.MonoCollect$CollectSubscriber.onComplete(MonoCollect.java:145)
        at reactor.core.publisher.FluxContextWriteRestoringThreadLocals$ContextWriteRestoringThreadLocalsSubscriber.onComplete(FluxContextWriteRestoringThreadLocals.java:149)
        at org.springframework.http.server.reactive.AbstractListenerReadPublisher$State.onAllDataRead(AbstractListenerReadPublisher.java:501)
        at org.springframework.http.server.reactive.AbstractListenerReadPublisher.onAllDataRead(AbstractListenerReadPublisher.java:137)
        at org.springframework.http.server.reactive.ServletServerHttpRequest$RequestBodyPublisher$RequestBodyPublisherReadListener.onAllDataRead(ServletServerHttpRequest.java:382)
        at org.eclipse.jetty.ee10.servlet.HttpInput.readCallback(HttpInput.java:370)
        at org.eclipse.jetty.server.handler.ContextHandler$ScopedContext.run(ContextHandler.java:1549)
        at org.eclipse.jetty.server.handler.ContextHandler$ScopedContext.run(ContextHandler.java:1542)
        at org.eclipse.jetty.ee10.servlet.ServletChannel.handle(ServletChannel.java:545)
        at org.eclipse.jetty.server.handler.ContextHandler$ScopedContext.run(ContextHandler.java:1555)
        at org.eclipse.jetty.server.handler.ContextHandler$ScopedContext.lambda$execute$0(ContextHandler.java:1572)
        at org.eclipse.jetty.util.thread.QueuedThreadPool.runJob(QueuedThreadPool.java:981)
        at org.eclipse.jetty.util.thread.QueuedThreadPool$Runner.doRunJob(QueuedThreadPool.java:1211)
        at org.eclipse.jetty.util.thread.QueuedThreadPool$Runner.run(QueuedThreadPool.java:1166)
        at java.base/java.lang.Thread.run(Thread.java:1474)
        at ua.raiffeisen.payments.infra.test.core.util.AssertUtil.assertEquals(AssertUtil.java:35)
        at ua.raiffeisen.payments.cardtransferinitiation.adapter.api.http.CardTransferInitiationIT.shouldRetrieveBadRequestErrorWhenHeaderRequestIdIsInvalid(CardTransferInitiationIT.java:317)
]]

--[[
]]

local trace = [[
        at ua.raiffeisen.payments.cardtransferinitiation.adapter.api.http.controller.CardTransferController$$SpringCGLIB$$0.initiateCardTransfer(<generated>.java:317)
]]

local parsed_trace = java_util.parse_java_mvn_run_class_text(trace)
for _, parsed in ipairs(parsed_trace) do
    print(parsed.class_path_root .. " - " .. parsed.method)
end
