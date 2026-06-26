local helper = require("tests.utils.spec_helper")

describe("modules.rust.test-report.nextest-xml", function()
    local nextest_xml
    local state
    local xml_parse
    local file_content

    --- Return a no-op logger for parser tests.
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
        file_content = "<testsuites/>"

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

        nextest_xml = helper.reload("modules.rust.test-report.nextest-xml")
    end)

    after_each(function()
        helper.clear_stub_modules({
            "modules.rust.test-report.nextest-xml",
            "utils.logging-util",
            "lib.file",
            "lib.xml",
        })
    end)

    it("lists XML reports from a nextest report directory", function()
        -- given
        vim.fn.glob = function(pattern, _, list)
            assert.are.equal("/repo/target/nextest/default/*.xml", pattern)
            assert.is_true(list)
            return { "/repo/target/nextest/default/junit.xml" }
        end

        -- when
        local files = nextest_xml.list_report_files("/repo/target/nextest/default")

        -- then
        assert.are.same({ "/repo/target/nextest/default/junit.xml" }, files)
    end)

    it("parses root, nested, failed, and skipped nextest cases", function()
        -- given
        xml_parse = function()
            return {
                testsuites = {
                    testsuite = {
                        _attr = { name = "payments_core" },
                        testcase = {
                            {
                                _attr = { name = "root_test", time = "0.1" },
                                ["system-out"] = "root metadata",
                            },
                            {
                                _attr = { name = "parser::inner::fails", time = "0.2" },
                                failure = {
                                    _attr = { message = "assertion failed" },
                                    "thread 'parser::inner::fails' panicked at src/parser.rs:42:9\n",
                                },
                                ["system-err"] = "stderr text",
                            },
                            {
                                _attr = { name = "parser::inner::skips", time = "0.3" },
                                skipped = {},
                            },
                        },
                    },
                },
            }
        end

        -- when
        local results = nextest_xml.parse_file("/repo/target/nextest/default/junit.xml")

        -- then
        assert.are.equal("passed", results["payments_core#root_test"].status)
        assert.are.equal("root_test", results["payments_core#root_test"].invocations[1].name)
        assert.are.equal("root metadata", results["payments_core#root_test"].invocations[1].metadata)
        assert.are.equal("failed", results["payments_core::parser::inner#fails"].status)
        assert.are.same(
            { { message = "assertion failed", line = 42 } },
            results["payments_core::parser::inner#fails"].errors
        )
        assert.are.equal("stderr text", results["payments_core::parser::inner#fails"].invocations[1].stderr)
        assert.are.equal("skipped", results["payments_core::parser::inner#skips"].status)
    end)

    it("uses the first panic line as an error message when failure attributes are missing", function()
        -- given
        xml_parse = function()
            return {
                testsuite = {
                    _attr = { name = "payments_core" },
                    testcase = {
                        _attr = { name = "parser::fails", time = "0.2" },
                        failure = "panicked at src/parser.rs:17:5\nstack backtrace:\n",
                    },
                },
            }
        end

        -- when
        local results = nextest_xml.parse_file("/repo/target/nextest/default/junit.xml")

        -- then
        local failure = results["payments_core::parser#fails"]
        assert.are.equal("failed", failure.status)
        assert.are.same({ { message = "panicked at src/parser.rs:17:5", line = 17 } }, failure.errors)
    end)

    it("returns empty results and notifies when a report file cannot be read", function()
        -- given
        helper.stub_module("lib.file", {
            read_file = function()
                return nil
            end,
        })
        nextest_xml = helper.reload("modules.rust.test-report.nextest-xml")

        -- when
        local results = nextest_xml.parse_file("/repo/target/nextest/default/missing.xml")

        -- then
        assert.are.same({}, results)
        assert.are.equal(
            "test-report: could not read file: /repo/target/nextest/default/missing.xml",
            state.notifications[1].message
        )
    end)

    it("combines parsed results from every report directory", function()
        -- given
        local parsed_by_file = {
            ["/repo/a/junit.xml"] = {
                testsuite = {
                    _attr = { name = "crate_a" },
                    testcase = { _attr = { name = "works", time = "0.1" } },
                },
            },
            ["/repo/b/junit.xml"] = {
                testsuite = {
                    _attr = { name = "crate_b" },
                    testcase = { _attr = { name = "works", time = "0.2" } },
                },
            },
        }
        vim.fn.glob = function(pattern)
            local report_dir = pattern:gsub("/%*%.xml$", "")
            return { report_dir .. "/junit.xml" }
        end
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
        nextest_xml = helper.reload("modules.rust.test-report.nextest-xml")

        -- when
        local results = nextest_xml.parse_results({ "/repo/a", "/repo/b" })

        -- then
        assert.are.equal("passed", results["crate_a#works"].status)
        assert.are.equal("passed", results["crate_b#works"].status)
    end)
end)
