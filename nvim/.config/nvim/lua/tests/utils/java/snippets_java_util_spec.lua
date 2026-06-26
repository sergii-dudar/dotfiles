local helper = require("tests.utils.spec_helper")

describe("utils.java.snippets-java-util", function()
    local snippets_util
    local state

    before_each(function()
        _, state = helper.reset_vim()
        snippets_util = helper.reload("utils.java.snippets-java-util")
    end)

    it("derives the current package from a Java source path", function()
        -- given
        state.buffer_names[0] = "/repo/src/main/java/ua/example/app/Foo.java"

        -- when
        local package_line = snippets_util.current_java_package()

        -- then
        assert.are.equal("package ua.example.app;", package_line)
    end)

    it("returns an empty package outside Java source roots", function()
        -- given
        state.buffer_names[0] = "/repo/README.md"

        -- when
        local package_line = snippets_util.current_java_package()

        -- then
        assert.are.equal("", package_line)
    end)

    it("returns the current Java file name with trailing snippet spacing", function()
        -- given
        state.buffer_names[0] = "/repo/src/main/java/ua/example/Foo.java"

        -- when
        local name = snippets_util.current_java_file_name()

        -- then
        assert.are.equal("Foo ", name)
    end)
end)
