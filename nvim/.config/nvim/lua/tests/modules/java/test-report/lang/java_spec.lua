local helper = require("tests.utils.spec_helper")

describe("modules.java.test-report.lang.java", function()
    local java_lang
    local parsed_files
    local report_files
    local java_common

    --- Return a no-op logger for Java test-report adapter tests.
    local function logger()
        return {
            debug = function() end,
            error = function() end,
            info = function() end,
            warn = function() end,
        }
    end

    before_each(function()
        helper.reset_vim()
        parsed_files = {}
        report_files = {}
        java_common = {
            java_class_to_proj_path = function(classname)
                return "/fallback/" .. classname:gsub("%.", "/") .. ".java"
            end,
            get_buffer_project_path = function()
                return "/repo/service"
            end,
            get_build_layout = function(module_path)
                return { report_dir = module_path .. "/target/junit-report" }
            end,
        }

        helper.stub_module("utils.logging-util", {
            new = logger,
        })
        helper.stub_module("utils.java.java-common", java_common)
        helper.stub_module("modules.java.test-report.junit-xml", {
            list_report_files = function(dir)
                return report_files[dir] or {}
            end,
            parse_file = function(filepath)
                return parsed_files[filepath] or {}
            end,
        })

        java_lang = helper.reload("modules.java.test-report.lang.java")
    end)

    after_each(function()
        helper.clear_stub_modules({
            "modules.java.test-report.lang.java",
            "modules.java.test-report.junit-xml",
            "utils.java.java-common",
            "utils.logging-util",
        })
    end)

    it("parses results from every configured report directory", function()
        -- given
        report_files["/repo/a/target/junit-report"] = { "/repo/a/TEST-A.xml" }
        report_files["/repo/b/target/junit-report"] = { "/repo/b/TEST-B.xml" }
        parsed_files["/repo/a/TEST-A.xml"] = {
            ["com.acme.ATest#works"] = { status = "passed" },
        }
        parsed_files["/repo/b/TEST-B.xml"] = {
            ["com.acme.BTest#fails"] = { status = "failed" },
        }

        -- when
        local results = java_lang.parse_results({
            "/repo/a/target/junit-report",
            "/repo/b/target/junit-report",
        })

        -- then
        assert.are.same({
            ["com.acme.ATest#works"] = { status = "passed" },
            ["com.acme.BTest#fails"] = { status = "failed" },
        }, results)
    end)

    it("splits a test id into display container, member, and package group", function()
        -- given
        local id = "com.acme.payment.FooTest#works"

        -- when
        local display = java_lang.id_to_display(id)

        -- then
        assert.are.same({
            container = "FooTest",
            member = "works",
            group = "com.acme.payment",
        }, display)
    end)

    it("uses the whole id as container when no method separator exists", function()
        -- given
        local id = "com.acme.payment.FooTest"

        -- when
        local display = java_lang.id_to_display(id)

        -- then
        assert.are.same({
            container = id,
            member = "",
            group = nil,
        }, display)
    end)

    it("resolves inner classes to the outer Java source file from the report project index", function()
        -- given
        vim.fn.glob = function(pattern, _, list)
            assert.are.equal("/repo/service/**/*.java", pattern)
            assert.is_true(list)
            return {
                "/repo/service/src/test/java/com/acme/FooTest.java",
                "/repo/service/src/test/java/com/acme/OtherTest.java",
            }
        end

        -- when
        local file = java_lang.id_to_file("com.acme.FooTest$Nested", "/repo/service/target/junit-report")

        -- then
        assert.are.equal("/repo/service/src/test/java/com/acme/FooTest.java", file)
    end)

    it("falls back to java_class_to_proj_path when the report project index has no match", function()
        -- given
        vim.fn.glob = function()
            return {}
        end

        -- when
        local file = java_lang.id_to_file("com.acme.MissingTest", "/repo/service/target/junit-report")

        -- then
        assert.are.equal("/fallback/com/acme/MissingTest.java", file)
    end)

    it("extracts a source line from a Java stacktrace", function()
        -- given
        local stacktrace = "java.lang.AssertionError\n\tat com.acme.FooTest.works(FooTest.java:31)\n"

        -- when
        local line = java_lang.extract_error_line("com.acme.FooTest", stacktrace)

        -- then
        assert.are.equal(31, line)
    end)

    it("returns the current module test report directory", function()
        -- given
        local expected = "/repo/service/target/junit-report"

        -- when
        local report_dir = java_lang.get_test_report_dir()

        -- then
        assert.are.equal(expected, report_dir)
    end)
end)
