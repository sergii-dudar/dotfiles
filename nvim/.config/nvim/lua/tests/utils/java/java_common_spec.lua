local helper = require("tests.utils.spec_helper")
local tmp_dir = helper.tmp_dir

describe("utils.java.java-common", function()
    local java_common
    local state

    local function write_file(path, content)
        local file = assert(io.open(path, "w"))
        file:write(content)
        file:close()
    end

    before_each(function()
        _, state = helper.reset_vim()
        helper.unload({ "utils.java.maven-util", "utils.java.java-common" })
        java_common = require("utils.java.java-common")
    end)

    it("parses a Java stack-trace frame", function()
        -- given
        local line = "    at ua.example.Outer$Inner.call(Outer.java:42)"

        -- when
        local parsed = java_common.parse_java_class_trace_line(line)

        -- then
        assert.are.equal("ua.example.Outer$Inner", parsed.class_path)
        assert.are.equal("ua.example.Outer", parsed.class_path_root)
        assert.are.equal("call", parsed.method)
        assert.are.equal(42, parsed.class_line_number)
    end)

    it("parses Maven compile diagnostics with zero-based positions", function()
        -- given
        local line = "[WARNING] /repo/src/main/java/Foo.java:[12,7] cannot find symbol"

        -- when
        local parsed = java_common.parse_mvn_compile_java_line(line)

        -- then
        assert.are.equal("/repo/src/main/java/Foo.java", parsed.file)
        assert.are.equal(11, parsed.lnum)
        assert.are.equal(6, parsed.col)
        assert.are.equal("cannot find symbol", parsed.message)
        assert.are.equal(vim.diagnostic.severity.WARN, parsed.severity)
    end)

    it("collects only parseable Maven compile lines from text", function()
        -- given
        local text = table.concat({
            "[INFO] nothing useful",
            "[ERROR] /repo/A.java:[1,2] first",
            "[ERROR] /repo/B.java:[3,4] second",
        }, "\n")

        -- when
        local parsed = java_common.parse_mvn_compile_java_text(text)

        -- then
        assert.are.equal(2, #parsed)
        assert.are.equal("/repo/A.java", parsed[1].file)
        assert.are.equal("/repo/B.java", parsed[2].file)
    end)

    it("matches abbreviated package segments against full package names", function()
        -- given
        local abbreviated = "r.c.p"
        local full = "reactor.core.publisher"

        -- when
        local matches = java_common.abbreviated_package_matches(abbreviated, full)
        local too_long = java_common.abbreviated_package_matches("r.c.p.extra", full)

        -- then
        assert.is_true(matches)
        assert.is_false(too_long)
    end)

    it("builds wildcard JDTLS queries from abbreviated class paths", function()
        -- given
        local class_path = "r.c.p.FluxFilterFuseable$Inner"

        -- when
        local query = java_common.build_abbreviated_query(class_path)

        -- then
        assert.are.equal("r*.c*.p*.FluxFilterFuseable", query)
    end)

    it("resolves class files directly or with abbreviated package fallback", function()
        -- given
        local glob_calls = {}
        vim.fn.glob = function(pattern)
            table.insert(glob_calls, pattern)
            if pattern == "*/**/ua/example/Foo.java" then
                return "/repo/src/main/java/ua/example/Foo.java"
            end
            if pattern == "*/**/u*/e*/Bar.java" then
                return "/repo/src/main/java/ua/example/Bar.java"
            end
            return ""
        end

        -- when
        local direct = java_common.java_class_to_proj_path("ua.example.Foo")
        local abbreviated = java_common.java_class_to_proj_path("u.e.Bar")

        -- then
        assert.are.equal("/repo/src/main/java/ua/example/Foo.java", direct)
        assert.are.equal("/repo/src/main/java/ua/example/Bar.java", abbreviated)
        assert.are.same({
            "*/**/ua/example/Foo.java",
            "*/**/u/e/Bar.java",
            "*/**/u*/e*/Bar.java",
        }, glob_calls)
    end)

    it("selects the nearest build tool marker for project type detection", function()
        -- given
        vim.fs.root = function(_, markers)
            if markers[1] == "pom.xml" then
                return "/repo"
            end
            return "/repo/module"
        end

        -- when
        local project_type = java_common.detect_project_type_at("/repo/module/src/Foo.java")

        -- then
        assert.are.equal("gradle", project_type)
    end)

    it("returns Gradle build layout while preferring existing JDTLS bin output", function()
        -- given
        vim.fs.root = function(_, markers)
            if markers[1] == "pom.xml" then
                return nil
            end
            return "/repo/module"
        end
        vim.fn.isdirectory = function(path)
            return path == "/repo/module/bin/main" and 1 or 0
        end

        -- when
        local layout = java_common.get_build_layout("/repo/module")

        -- then
        assert.are.equal("gradle", layout.tool)
        assert.are.equal("/repo/module/bin/main", layout.classes_dir)
        assert.are.equal("/repo/module/build/classes/java/test", layout.test_classes_dir)
    end)

    it("builds an FQCN from a Java file package declaration", function()
        -- given
        local file_path = tmp_dir .. "Foo.java"
        write_file(file_path, "package ua.example;\n\npublic class Foo {}\n")

        -- when
        local fqn = java_common.file_to_fqcn(file_path)

        -- then
        assert.are.equal("ua.example.Foo", fqn)
    end)

    it("detects test source files by path", function()
        -- given
        state.buffer_names[7] = "/repo/src/test/java/ua/example/FooTest.java"
        state.buffer_names[8] = "/repo/src/main/java/ua/example/Foo.java"

        -- when
        local test_file = java_common.is_test_file(7)
        local main_file = java_common.is_test_file(8)

        -- then
        assert.is_true(test_file)
        assert.is_false(main_file)
    end)
end)
