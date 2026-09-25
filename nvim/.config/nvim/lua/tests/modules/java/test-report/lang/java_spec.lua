local helper = require("tests.utils.spec_helper")

describe("modules.java.test-report.lang.java", function()
    local java_lang
    local parsed_files
    local report_files
    local java_common
    local walk_entries
    local walk_calls

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
        -- The adapter delegates error-line extraction to the real parser helper.
        local real_junit_xml = helper.reload("modules.java.test-report.junit-xml")
        helper.stub_module("modules.java.test-report.junit-xml", {
            list_report_files = function(dir)
                return report_files[dir] or {}
            end,
            parse_file = function(filepath)
                return parsed_files[filepath] or {}
            end,
            _extract_error_line = real_junit_xml._extract_error_line,
        })

        -- Filesystem walk used by the class index (vim.fs.dir yields <relative name, type>).
        walk_entries = {}
        walk_calls = 0
        vim.fs.dir = function(_, _)
            walk_calls = walk_calls + 1
            local entries = walk_entries
            local i = 0
            return function()
                i = i + 1
                local e = entries[i]
                if e then
                    return e[1], e[2]
                end
                return nil
            end
        end
        vim.fs.basename = function(path)
            return path:match("([^/]+)$") or path
        end
        vim.uv.fs_realpath = function(path)
            return path
        end
        vim.split = vim.split
            or function(s, sep)
                local out = {}
                for part in (s .. sep):gmatch("(.-)" .. sep:gsub("%p", "%%%0")) do
                    table.insert(out, part)
                end
                return out
            end

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
        walk_entries = {
            { "src/test/java/com/acme/FooTest.java", "file" },
            { "src/test/java/com/acme/OtherTest.java", "file" },
            { "src/main/java/com/acme/Foo.java", "file" },
        }

        -- when
        local file = java_lang.id_to_file("com.acme.FooTest$Nested", "/repo/service/target/junit-report")

        -- then
        assert.are.equal("/repo/service/src/test/java/com/acme/FooTest.java", file)
        assert.are.equal(1, walk_calls)
    end)

    it("prefers the shallowest path when nested modules carry the same source suffix", function()
        -- given: walk order is filesystem order, the choice must not depend on it
        walk_entries = {
            { "legacy/inner/src/test/java/com/acme/FooTest.java", "file" },
            { "src/test/java/com/acme/FooTest.java", "file" },
        }

        -- when
        local file = java_lang.id_to_file("com.acme.FooTest", "/repo/service/target/junit-report")

        -- then
        assert.are.equal("/repo/service/src/test/java/com/acme/FooTest.java", file)
    end)

    it("rebuilds the index once for a class created after the first run, then remembers the miss", function()
        -- given
        walk_entries = { { "src/test/java/com/acme/FooTest.java", "file" } }
        assert.is_not_nil(java_lang.id_to_file("com.acme.FooTest", "/repo/service/target/junit-report"))
        assert.are.equal(1, walk_calls)

        -- when: a new test class appears on disk
        walk_entries = {
            { "src/test/java/com/acme/FooTest.java", "file" },
            { "src/test/java/com/acme/NewTest.java", "file" },
        }
        local found = java_lang.id_to_file("com.acme.NewTest", "/repo/service/target/junit-report")

        -- then: one rebuild resolved it
        assert.are.equal("/repo/service/src/test/java/com/acme/NewTest.java", found)
        assert.are.equal(2, walk_calls)

        -- and: a genuinely missing class triggers one rebuild, not one per lookup
        java_common.java_class_to_proj_path = function()
            return nil
        end
        assert.is_nil(java_lang.id_to_file("com.acme.GhostTest", "/repo/service/target/junit-report"))
        assert.is_nil(java_lang.id_to_file("com.acme.GhostTest", "/repo/service/target/junit-report"))
        assert.are.equal(3, walk_calls)
    end)

    it("falls back to java_class_to_proj_path when the report project index has no match", function()
        -- given
        walk_entries = {}

        -- when
        local file = java_lang.id_to_file("com.acme.MissingTest", "/repo/service/target/junit-report")

        -- then
        assert.are.equal("/fallback/com/acme/MissingTest.java", file)
    end)

    it("takes the first match when the fallback glob returns several newline-joined paths", function()
        -- given
        walk_entries = {}
        java_common.java_class_to_proj_path = function()
            return "/a/src/test/java/com/acme/DupTest.java\n/b/src/test/java/com/acme/DupTest.java"
        end

        -- when
        local file = java_lang.id_to_file("com.acme.DupTest", "/repo/service/target/junit-report")

        -- then
        assert.are.equal("/a/src/test/java/com/acme/DupTest.java", file)
    end)

    it("extracts the error line for a nested class from the outer source file frame", function()
        -- given
        local stacktrace = "java.lang.AssertionError\n\tat com.acme.FooTest$Inner.works(FooTest.java:31)\n"

        -- when
        local line = java_lang.extract_error_line("com.acme.FooTest$Inner", stacktrace)

        -- then
        assert.are.equal(31, line)
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
