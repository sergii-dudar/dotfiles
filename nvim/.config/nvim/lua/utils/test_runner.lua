local module = require("utils.java.maven-tests2")

local test_out = [[WARNING: A terminally deprecated method in sun.misc.Unsafe has been called
WARNING: sun.misc.Unsafe::staticFieldBase has been called by com.google.inject.internal.aop.HiddenClassDefiner (file:/home/serhii/.sdkman/candidates/maven/current/lib/guice-5
.1.0-classes.jar)
WARNING: Please consider reporting this to the maintainers of class com.google.inject.internal.aop.HiddenClassDefiner
WARNING: sun.misc.Unsafe::staticFieldBase will be removed in a future release
[ERROR] Tests run: 2, Failures: 2, Errors: 0, Skipped: 0, Time elapsed: 0.075 s <<< FAILURE! -- in ua.serhii.application.Tests1
[ERROR] ua.serhii.application.Tests1.testSomething1 -- Time elapsed: 0.053 s <<< FAILURE!
org.opentest4j.AssertionFailedError: 

expected: 1
 but was: 2
        at ua.serhii.application.Tests1.testSomething1(Tests1.java:17)
 
[ERROR] ua.serhii.application.Tests1.testSomething -- Time elapsed: 0.006 s <<< FAILURE!
org.opentest4j.AssertionFailedError: 

expected: 3
 but was: 4
        at ua.serhii.application.Tests1.testSomething(Tests1.java:11)

[ERROR] Tests run: 2, Failures: 2, Errors: 0, Skipped: 0, Time elapsed: 0.005 s <<< FAILURE! -- in ua.serhii.application.Tests2
[ERROR] ua.serhii.application.Tests2.testSomething2 -- Time elapsed: 0.001 s <<< FAILURE!
org.opentest4j.AssertionFailedError: 

expected: 5
 but was: 6
        at ua.serhii.application.Tests2.testSomething2(Tests2.java:12)

[ERROR] ua.serhii.application.Tests2.testSomething3 -- Time elapsed: 0.001 s <<< FAILURE!
java.lang.AssertionError: some error to note
        at ua.serhii.application.test.utils.TestUtil.assertThat(TestUtil.java:10)
        at ua.serhii.application.Tests2.testAssdrt(Tests2.java:23)
        at ua.serhii.application.Tests2.testSomething3(Tests2.java:18)

[ERROR] Failures: 
[ERROR]   Tests1.testSomething:11 
expected: 7
 but was: 8
[ERROR]   Tests1.testSomething1:17 
expected: "23"
 but was: "22"
[ERROR]   Tests2.testSomething2:12 
expected: 5
 but was: 4
[ERROR]   Tests2.testSomething3:18->testAssdrt:23 some error to note
[ERROR] Tests run: 4, Failures: 4, Errors: 0, Skipped: 0
[ERROR] Failed to execute goal org.apache.maven.plugins:maven-surefire-plugin:3.2.5:test (default-test) on project serhii-application: There are test failures.
[ERROR] 
[ERROR] Please refer to /home/serhii/serhii.home/git/tests/serhii-application/target/surefire-reports for the individual test results.
[ERROR] Please refer to dump files (if any exist) [date].dump, [date]-jvmRun[N].dump and [date].dumpstream.
[ERROR] -> [Help 1]
[ERROR] 
[ERROR] To see the full stack trace of the errors, re-run Maven with the -e switch.
[ERROR] Re-run Maven using the -X switch to enable full debug logging.
[ERROR] 
[ERROR] For more information about the errors and possible solutions, please read the following articles:
[ERROR] [Help 1] http://cwiki.apache.org/confluence/display/MAVEN/MojoFailureException

[Process exited 1]
]]

dd(module.parse_maven_output(test_out))
--parse_maven_output(test_out)

-- publish_maven_diagnostics(test_out)

------------------------------------
