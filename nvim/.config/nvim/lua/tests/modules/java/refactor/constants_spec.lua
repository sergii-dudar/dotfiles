local helper = require("tests.utils.spec_helper")

describe("modules.java.refactor.constants", function()
    local constants
    local build_files

    --- Return a no-op logger for refactor module tests.
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
        build_files = {}
        vim.fn.filereadable = function(path)
            return build_files[path] and 1 or 0
        end

        helper.stub_module("utils.logging-util", {
            new = logger,
        })
        constants = helper.reload("modules.java.refactor.constants")
    end)

    after_each(function()
        helper.clear_stub_modules({
            "modules.java.refactor.constants",
            "utils.logging-util",
        })
    end)

    it("escapes single quotes for shell-safe single-quoted strings", function()
        -- given
        local value = "/tmp/a'b/Foo.java"

        -- when
        local escaped = constants.shell_escape(value)

        -- then
        assert.are.equal("'/tmp/a'\\''b/Foo.java'", escaped)
    end)

    it("builds a two-pass sed expression for Java type replacement", function()
        -- given
        local old_name = "OldType"
        local new_name = "NewType"

        -- when
        local expression = constants.build_type_replace_expr(old_name, new_name)

        -- then
        assert.matches("OldType", expression)
        assert.matches("NewType", expression)
        assert.matches("; ", expression)
    end)

    it("detects a module path by walking up to a Maven or Gradle build file", function()
        -- given
        build_files["/repo/service/pom.xml"] = true
        local file_path = "/repo/service/src/main/java/com/acme/Foo.java"

        -- when
        local module_path = constants.detect_module_path(file_path)

        -- then
        assert.are.equal("/repo/service", module_path)
    end)

    it("falls back to the path before src kind and java when no build file exists", function()
        -- given
        local file_path = "/repo/service/src/test/java/com/acme/FooTest.java"

        -- when
        local module_path = constants.detect_module_path(file_path)

        -- then
        assert.are.equal("/repo/service", module_path)
    end)

    it("extracts a package-relative path for a configured source root", function()
        -- given
        local full_path = "/repo/service/src/main/java/com/acme/Foo.java"

        -- when
        local package_path = constants.get_package_path(full_path, constants.main_dir)

        -- then
        assert.are.equal("com/acme/Foo.java", package_path)
    end)
end)
