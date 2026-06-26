local helper = require("tests.utils.spec_helper")

describe("modules.java.test-report.junit-xml", function()
    local junit_xml
    local state
    local xml_parse
    local file_content

    --- Return a no-op logger for parser module tests.
    local function logger()
        return {
            debug = function() end,
            error = function() end,
            info = function() end,
            warn = function() end,
        }
    end

    before_each(function()
        _, state = helper.reset_vim()
        xml_parse = function()
            return {}
        end
        file_content = "<testsuite/>"

        helper.stub_module("utils.logging-util", {
            new = logger,
        })
        helper.stub_module("lib.file", {
            read_file = function()
                return file_content
            end,
        })
        helper.stub_module("lib.xml", {
            parse = function(content)
                return xml_parse(content)
            end,
        })

        junit_xml = helper.reload("modules.java.test-report.junit-xml")
    end)

    after_each(function()
        helper.clear_stub_modules({
            "modules.java.test-report.junit-xml",
            "utils.logging-util",
            "lib.file",
            "lib.xml",
        })
    end)

    it("lists JUnit XML reports and warns when none exist", function()
        -- given
        vim.fn.glob = function(pattern, _, list)
            assert.are.equal("/repo/target/junit-report/TEST-*.xml", pattern)
            assert.is_true(list)
            return {}
        end

        -- when
        local files = junit_xml.list_report_files("/repo/target/junit-report")

        -- then
        assert.are.same({}, files)
        assert.are.equal("No JUnit XML reports found in: /repo/target/junit-report", state.notifications[1].message)
        assert.are.equal(vim.log.levels.WARN, state.notifications[1].level)
    end)

    it("parses passed, failed, and skipped test cases from one report file", function()
        -- given
        xml_parse = function()
            return {
                testsuite = {
                    testcase = {
                        {
                            _attr = { classname = "com.acme.FooTest", name = "passes()", time = "0.1" },
                            ["system-out"] = {
                                "unique-id: passes",
                                "stdout line 1\n",
                                { "stdout line 2" },
                            },
                        },
                        {
                            _attr = { classname = "com.acme.FooTest", name = "fails()", time = "0.2" },
                            failure = {
                                _attr = { message = "expected true" },
                                "org.opentest4j.AssertionFailedError: expected true\n"
                                    .. "\tat com.acme.FooTest.fails(FooTest.java:42)\n",
                            },
                            ["system-err"] = "stderr text",
                        },
                        {
                            _attr = { classname = "com.acme.FooTest", name = "skips()", time = "0.3" },
                            skipped = {},
                        },
                    },
                },
            }
        end

        -- when
        local results = junit_xml.parse_file("/repo/target/junit-report/TEST-FooTest.xml")

        -- then
        assert.are.equal("passed", results["com.acme.FooTest#passes"].status)
        assert.are.equal("unique-id: passes", results["com.acme.FooTest#passes"].invocations[1].metadata)
        assert.are.equal("stdout line 1\nstdout line 2", results["com.acme.FooTest#passes"].invocations[1].stdout)
        assert.are.equal("failed", results["com.acme.FooTest#fails"].status)
        assert.are.same({ { message = "expected true", line = 42 } }, results["com.acme.FooTest#fails"].errors)
        assert.are.equal("stderr text", results["com.acme.FooTest#fails"].invocations[1].stderr)
        assert.are.equal("skipped", results["com.acme.FooTest#skips"].status)
    end)

    it("merges parameterized invocations under the base method id", function()
        -- given
        local results = {}
        local testsuite = {
            testcase = {
                {
                    _attr = { classname = "com.acme.FooTest", name = "sample()[1]", time = "0.1" },
                },
                {
                    _attr = { classname = "com.acme.FooTest", name = "sample()[2]", time = "0.2" },
                    failure = {
                        _attr = { message = "boom" },
                        "java.lang.AssertionError: boom\n\tat com.acme.FooTest.sample(FooTest.java:77)\n",
                    },
                },
            },
        }

        -- when
        junit_xml._process_testsuite(testsuite, results)

        -- then
        local result = results["com.acme.FooTest#sample"]
        assert.are.equal("failed", result.status)
        assert.are.equal(0.3, tonumber(string.format("%.1f", result.time)))
        assert.are.same({ { message = "boom", line = 77 } }, result.errors)
        assert.are.same({ "sample()[1]", "sample()[2]" }, {
            result.invocations[1].name,
            result.invocations[2].name,
        })
    end)

    it("recovers a failure message from stacktrace text when XML attributes are incomplete", function()
        -- given
        local failure = "org.opentest4j.AssertionFailedError: expected: <2> but was: <1>\n"
            .. "\tat com.acme.FooTest.fails(FooTest.java:13)\n"

        -- when
        local errors, stacktrace = junit_xml._extract_errors("com.acme.FooTest", failure)

        -- then
        assert.matches("AssertionFailedError", errors[1].message)
        assert.are.equal(13, errors[1].line)
        assert.matches("FooTest.java:13", stacktrace)
    end)

    it("combines reports from a directory with later files overriding duplicate ids", function()
        -- given
        vim.fn.glob = function()
            return {
                "/repo/target/junit-report/TEST-First.xml",
                "/repo/target/junit-report/TEST-Second.xml",
            }
        end
        local parsed_by_file = {
            ["/repo/target/junit-report/TEST-First.xml"] = {
                testsuite = {
                    testcase = {
                        _attr = { classname = "com.acme.FooTest", name = "same()", time = "0.1" },
                    },
                },
            },
            ["/repo/target/junit-report/TEST-Second.xml"] = {
                testsuite = {
                    testcase = {
                        _attr = { classname = "com.acme.FooTest", name = "same()", time = "0.2" },
                        failure = { _attr = { message = "later" }, "at com.acme.FooTest.same(FooTest.java:9)" },
                    },
                },
            },
        }
        helper.stub_module("lib.file", {
            read_file = function(filepath)
                return filepath
            end,
        })
        helper.stub_module("lib.xml", {
            parse = function(filepath)
                return parsed_by_file[filepath]
            end,
        })
        junit_xml = helper.reload("modules.java.test-report.junit-xml")

        -- when
        local results = junit_xml.parse_report_dir("/repo/target/junit-report")

        -- then
        assert.are.equal("failed", results["com.acme.FooTest#same"].status)
        assert.are.equal(0.2, results["com.acme.FooTest#same"].time)
    end)
end)
